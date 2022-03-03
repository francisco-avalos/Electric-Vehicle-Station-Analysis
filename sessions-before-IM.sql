
DROP PROCEDURE IF EXISTS my_schema.fa_stations_out_thesis;
DELIMITER $$
-- CREATE PROCEDURE my_schema.fa_stations_out_thesis(IN start_date DATE, end_date DATE, market_parameter VARCHAR(50))
CREATE PROCEDURE my_schema.fa_stations_out_thesis(IN start_date DATE, end_date DATE, state VARCHAR(50))
BEGIN 
	DROP TEMPORARY TABLE IF EXISTS my_schema.la_jan_daily;
	CREATE TEMPORARY TABLE my_schema.la_jan_daily
	SELECT
		DISTINCT S.station_id,
		S.station_socket_id,
		S.station_socket,
		DATE(DM.flowdate) AS flowdate,
		24*60 AS possible_min,
        SMD.charger_company AS charger_company_col,
        S.station_latitude,
        S.station_longitude
	FROM my_schema.station S
	JOIN my_schema2.date_matrix DM ON DATE(DM.flowdate) BETWEEN S.station_usage_start_date AND IFNULL(S.station_decommission_date,ADDDATE(CURDATE(), INTERVAL 5 YEAR))
	JOIN dma DMA ON DMA.zip = S.station_zip
		AND DMA.active = 1
	JOIN my_schema.station_model_detail SMD ON S.station_model = SMD.station_model
	WHERE DATE(DM.flowdate) BETWEEN start_date AND LAST_DAY(end_date)
		AND S.host_id NOT IN (215,364,423,429,76)
-- 		AND DMA.dma_name = market_parameter
		AND S.station_state = state
		AND S.station_non_networked = 0
		AND S.station_excluded = 0
		AND S.station_managed = 1
		AND S.station_deleted = 0
	;
    
 	DROP TEMPORARY TABLE IF EXISTS my_schema.final_IM_minutes_both_connectors;
 	CREATE TEMPORARY TABLE my_schema.final_IM_minutes_both_connectors 
 	(
	   `ID` INT AUTO_INCREMENT,
 	   `date` DATE DEFAULT NULL,
 	   `station_id` bigint(20) DEFAULT NULL,
 	   `im_start` varchar(21) DEFAULT NULL,
 	   `im_end` varchar(24) DEFAULT NULL,
 	   `minutes_diff` bigint(21) DEFAULT NULL,
       PRIMARY KEY (ID, date, station_id),
       INDEX(date, station_id)
 	);
 	SET @isdate := start_date;
	SET @enddate := LAST_DAY(end_date);
     
	WHILE @isdate <= @enddate DO

 		DROP TEMPORARY TABLE IF EXISTS my_schema.one_IM_day;
 		CREATE TEMPORARY TABLE my_schema.one_IM_day
		SELECT 
			LJD.flowdate AS ref_date,
            LJD.station_id,
            LJD.station_socket_id,
 			DATE_FORMAT(IM.in_maintenance_start, '%Y-%m-%d %H:%i') AS in_maintenance_start,
 			DATE_FORMAT(IM.adjusted_in_maintenance_start, '%Y-%m-%d %H:%i') AS adjusted_in_maintenance_start,
 			DATE_FORMAT(IM.in_maintenance_end, '%Y-%m-%d %H:%i') AS in_maintenance_end,
 			DATE_FORMAT(IM.adjusted_in_maintenance_end, '%Y-%m-%d %H:%i') AS adjusted_in_maintenance_end,
            IM.in_maintenance_minutes,
			charger_company_col
		FROM my_schema.la_jan_daily LJD
        LEFT JOIN my_schema.in_maintenance IM ON IM.station_socket_id = LJD.station_socket_id
			AND IM.ref_date = LJD.flowdate
		WHERE LJD.flowdate = @isdate
		;
 
		UPDATE my_schema.one_IM_day IM
 		JOIN my_schema.station_maintenance_exclusion SME ON IM.station_socket_id = SME.station_socket_id 
 			AND IM.ref_date = SME.ref_date
 		SET IM.in_maintenance_minutes = 0
 		WHERE SME.Internal_External = 'E';

        # Scenario 1
 		DROP TEMPORARY TABLE IF EXISTS my_schema.same_IM_start_end_time;
 		CREATE TEMPORARY TABLE my_schema.same_IM_start_end_time
 		SELECT 
 			IM.ref_date,
 			IM.station_id,
 			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS in_maintenance_start_min,
 			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS in_maintenance_end_min,
 			COUNT(DISTINCT IM.station_socket_id) AS station_sockets,
            IM.in_maintenance_minutes AS in_maintenance_minutes_col
 		FROM my_schema.one_IM_day IM 
 		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
        WHERE in_maintenance_minutes != 0 
 		GROUP BY in_maintenance_start_min, in_maintenance_end_min, station_id
 		HAVING station_sockets > 1;
		
        # Identify all other NON-scenario 1s
 		DROP TEMPORARY TABLE IF EXISTS my_schema.other_ims;
 		CREATE TEMPORARY TABLE my_schema.other_ims
 		SELECT
 			A.ref_date,
 			A.station_id,
			COUNT(DISTINCT IF(IFNULL(A.adjusted_in_maintenance_start, A.in_maintenance_start) IS NOT NULL
									AND IFNULL(A.adjusted_in_maintenance_end, A.in_maintenance_end) IS NOT NULL, station_socket_id, NULL)) AS connectors,
			MAX(in_maintenance_minutes) AS max_im
--  			,COUNT(DISTINCT station_socket_id) AS connectors # updated 
 		FROM my_schema.one_IM_day A
 		LEFT JOIN my_schema.same_IM_start_end_time B ON A.station_id = B.station_id
 			AND IFNULL(A.adjusted_in_maintenance_start, A.in_maintenance_start) = B.in_maintenance_start_min
 			AND IFNULL(A.adjusted_in_maintenance_end, A.in_maintenance_end) = B.in_maintenance_end_min
 		WHERE B.station_id IS NULL
 		GROUP BY station_id, A.ref_date
 		HAVING connectors > 1
			AND max_im > 1;
 
 		## Scenario # 4
 		DROP TEMPORARY TABLE IF EXISTS my_schema.other_minutes;
 		CREATE TEMPORARY TABLE my_schema.other_minutes
 		SELECT
 			IM.station_id,
 			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
 			MIN(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s')) AS im_end,
 			TIMESTAMPDIFF(MINUTE, DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s'), MIN(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s'))) AS minutes_diff
 
 		FROM my_schema.in_maintenance IM
 		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
 		JOIN 
 			(
 				## collects scenario 4
 				SELECT
 					IM.ref_date,
 					IM.station_id,
 					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
 					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
 					COUNT(*) AS counts
 				FROM my_schema.one_IM_day IM
 				JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
 				JOIN my_schema.other_ims OI ON OI.ref_date = IM.ref_date
 					AND OI.station_id = IM.station_id
 				WHERE IM.in_maintenance_minutes > 0
					AND S.host_id NOT IN (215,364,423,429,76)
 				GROUP BY DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i'), IM.station_id
 				HAVING counts > 1
 			) X ON IM.station_id = X.station_id
 					AND IM.ref_date = X.ref_date
 					AND DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') = X.im_start
 		GROUP BY IM.station_id;
 
 		## Scenario # 5
 		INSERT INTO my_schema.other_minutes
 		SELECT
			S.station_id,
			MAX(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s')) AS im_start,
			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
			TIMESTAMPDIFF(MINUTE, MAX(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s')), DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s')) AS minutes_diff
 
 		FROM my_schema.in_maintenance IM
 		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
 		JOIN
 			(
 				## collects scenario 5
 				SELECT
 					IM.ref_date,
 					IM.station_id,
 					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
 					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
 					COUNT(*) AS counts
 				FROM my_schema.one_IM_day IM
 				JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
 				JOIN my_schema.other_ims OI ON OI.ref_date = IM.ref_date
 					AND OI.station_id = IM.station_id
 				LEFT JOIN my_schema.other_minutes OM ON OM.station_id = IM.station_id
 					AND OM.im_start = DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i')
 				WHERE OM.station_id IS NULL
 					AND IM.in_maintenance_minutes > 0
					AND S.host_id NOT IN (215,364,423,429,76)
 				GROUP BY DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i'), IM.station_id
 				HAVING counts > 1
 			) X ON IM.station_id = X.station_id
 					AND IM.ref_date = X.ref_date
 					AND DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') = X.im_end
 		GROUP BY IM.station_id;
 
		# Covers scenarios 2, 3, 6: connectors w/ diff im-start and im-end and have some overlap
		DROP TEMPORARY TABLE IF EXISTS my_schema.scenarios_236;
		CREATE TEMPORARY TABLE my_schema.scenarios_236
		SELECT 
			IM.ref_date,
			IM.station_id,
			1440 - IFNULL(IF(TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) = 0, 1440, 
					TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)))), 0) AS x_0,
			IFNULL(TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), DATE_FORMAT(MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), '%Y-%m-%d 00:00')), 0) AS x_1,
			
			TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) AS a,
			TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), MAX(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start))) AS b,
			
			1440 - (1440 - IFNULL(IF(TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) = 0, 1440, 
					TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)))), 0) +
					IFNULL(TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), DATE_FORMAT(MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), '%Y-%m-%d 00:00')), 0) +
					TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) +
					TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), MAX(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)))) AS overlapping_minutes_both_connectors_IM
		FROM my_schema.one_IM_day IM
		JOIN (
				SELECT A.*
				FROM my_schema.other_ims A
				LEFT JOIN my_schema.other_minutes B ON A.station_id = B.station_id
				WHERE B.station_id IS NULL
			) OT ON OT.ref_date = IM.ref_date
				AND OT.station_id = IM.station_id
		WHERE IM.in_maintenance_minutes != 0
		GROUP BY IM.ref_date, IM.station_id;


 		INSERT INTO my_schema.final_IM_minutes_both_connectors (date, station_id, im_start, im_end, minutes_diff)
        # both connectors start/end IM
 		SELECT
 			@isdate AS date,
 			station_id,
 			in_maintenance_start_min AS im_start,
 			in_maintenance_end_min AS im_end,
 			in_maintenance_minutes_col AS minutes_diff
--  			TIMESTAMPDIFF(MINUTE, in_maintenance_start_min, in_maintenance_end_min) AS minutes_diff
 		FROM my_schema.same_IM_start_end_time
 		UNION ALL
        # both connectors start IM or end IM at the same time
 		SELECT
 			@isdate AS date, 
 			A.*
		FROM my_schema.other_minutes A
        UNION ALL
		# each connector has different start IM and different end IM 
        SELECT 
			@isdate AS date,
			station_id,
			ref_date AS im_start, # doesn't matter
			ref_date AS im_end, # doesn't matter
			SUM(overlapping_minutes_both_connectors_IM) AS minutes_diff
		FROM my_schema.scenarios_236
		GROUP BY station_id;
        
		SET @isdate = ADDDATE(@isdate, INTERVAL 1 DAY);
	END WHILE;

	DROP TEMPORARY TABLE IF EXISTS my_schema.IM_minutes_by_day_station;
	CREATE TEMPORARY TABLE my_schema.IM_minutes_by_day_station
	SELECT 
		date,
		station_id,
		SUM(minutes_diff) AS IM_minutes,
		1440 AS possible_minutes,
		1440 - SUM(minutes_diff) AS NON_IM_minutes,
		ROUND(SUM(minutes_diff / 60), 2) AS IM_hours,
		24 AS possible_hours,
		ROUND(24 - SUM(minutes_diff / 60), 2) AS NON_IM_hours
	FROM my_schema.final_IM_minutes_both_connectors
	GROUP BY date, station_id;

END $$
DELIMITER ;

CALL my_schema.fa_stations_out_thesis('2019-01-01', '2020-12-01', 'FL');


-- UPDATE my_schema.la_jan_daily A
-- SET A.station_id = "DELETE ME"
-- WHERE A.station_id NOT IN (SELECT DISTINCT station_id FROM my_schema.final_IM_minutes_both_connectors WHERE minutes_diff > 360);

-- DELETE FROM my_schema.la_jan_daily
-- WHERE station_id = 'DELETE ME';


ALTER TABLE my_schema.la_jan_daily
ADD PRIMARY KEY (flowdate, station_socket_id),
ADD INDEX(flowdate, station_id);


-- DROP TEMPORARY TABLE IF EXISTS my_schema.station_im_periods;
-- CREATE TEMPORARY TABLE my_schema.station_im_periods
-- SELECT 
-- 	A.flowdate,
--     A.station_id,
--     A.charger_company_col,
--     B.date AS im_date,
--     B.im_start,
--     B.im_end,
--     B.minutes_diff,
-- 	IF(B.date IS NULL, 0, 1) AS indicator,
--     MIN(B.date) OVER(PARTITION BY B.station_id, IF(B.date IS NULL, 0, 1)) AS first_im_date_by_station, # this gets me the first IM day ever in the data for a given station
--     RANK() OVER(PARTITION BY B.station_id ORDER BY B.date) AS days,
--     IF(ND.date IS NOT NULL AND DB.date IS NULL, A.flowdate, NULL) AS starting_im_period,
--     IF(ND.date IS NULL AND DB.date IS NOT NULL, A.flowdate, NULL) AS end_im_period,
--     IF(B.date IS NOT NULL AND ND.date IS NULL AND DB.date IS NULL, A.flowdate, NULL) AS single_IM_day

-- FROM my_schema.la_jan_daily A
-- LEFT JOIN my_schema.final_IM_minutes_both_connectors B ON A.flowdate = B.date
-- 	AND A.station_id = B.station_id
-- LEFT JOIN my_schema.final_IM_minutes_both_connectors ND ON B.station_id = ND.station_id
-- 	AND ADDDATE(B.date, INTERVAL 1 DAY) = ND.date
-- LEFT JOIN my_schema.final_IM_minutes_both_connectors DB ON B.station_id = DB.station_id
-- 	AND SUBDATE(B.date, INTERVAL 1 DAY) = DB.date
-- GROUP BY A.station_id, A.flowdate;

DROP TEMPORARY TABLE IF EXISTS my_schema.station_im_periods;
CREATE TEMPORARY TABLE my_schema.station_im_periods
(
   `flowdate` date DEFAULT NULL,
   `station_id` bigint(20) NOT NULL,
   `charger_company_col` varchar(16) DEFAULT NULL,
   `station_latitude` varchar(16) NOT NULL,
   `station_longitude` varchar(16) NOT NULL,
   `im_date` date,
   `im_start` varchar(21) DEFAULT NULL,
   `im_end` varchar(24) DEFAULT NULL,
   `minutes_diff` bigint(21) DEFAULT NULL,
   `indicator` int(1) NOT NULL,
   `first_im_date_by_station` date DEFAULT NULL,
   `days` bigint(21) NOT NULL,
   `starting_im_period` date DEFAULT NULL,
   `end_im_period` date DEFAULT NULL,
   `single_IM_day` date DEFAULT NULL
 );

DROP PROCEDURE IF EXISTS my_schema.fa_get_sessions_for_im_stations_thesis_im_periods;
DELIMITER $$
CREATE PROCEDURE my_schema.fa_get_sessions_for_im_stations_thesis_im_periods()
BEGIN 
	DROP TEMPORARY TABLE IF EXISTS my_schema.station_tracker;
	CREATE TEMPORARY TABLE my_schema.station_tracker
	SELECT station_id FROM my_schema.la_jan_daily GROUP BY 1;
    SET @station_id := (SELECT MIN(station_id) FROM my_schema.station_tracker);
    
    WHILE @station_id IS NOT NULL DO
		REPLACE INTO my_schema.station_im_periods
		SELECT 
			A.flowdate,
			A.station_id,
			A.charger_company_col,
			A.station_latitude,
			A.station_longitude,
			B.date AS im_date,
			B.im_start,
			B.im_end,
			B.minutes_diff,
			IF(B.date IS NULL, 0, 1) AS indicator,
			MIN(B.date) OVER(PARTITION BY B.station_id, IF(B.date IS NULL, 0, 1)) AS first_im_date_by_station, # this gets me the first IM day ever in the data for a given station
			RANK() OVER(PARTITION BY B.station_id ORDER BY B.date) AS days,
			IF(ND.date IS NOT NULL AND DB.date IS NULL, A.flowdate, NULL) AS starting_im_period,
			IF(ND.date IS NULL AND DB.date IS NOT NULL, A.flowdate, NULL) AS end_im_period,
			IF(B.date IS NOT NULL AND ND.date IS NULL AND DB.date IS NULL, A.flowdate, NULL) AS single_IM_day

		FROM my_schema.la_jan_daily A
		LEFT JOIN my_schema.final_IM_minutes_both_connectors B ON A.flowdate = B.date
			AND A.station_id = B.station_id
		LEFT JOIN my_schema.final_IM_minutes_both_connectors ND ON B.station_id = ND.station_id
			AND ADDDATE(B.date, INTERVAL 1 DAY) = ND.date
		LEFT JOIN my_schema.final_IM_minutes_both_connectors DB ON B.station_id = DB.station_id
			AND SUBDATE(B.date, INTERVAL 1 DAY) = DB.date
		WHERE A.station_id = @station_id
		GROUP BY A.station_id, A.flowdate;

        DELETE FROM my_schema.station_tracker WHERE station_id = @station_id;
		SET @station_id := (SELECT MIN(station_id) FROM my_schema.station_tracker);
	END WHILE;
END $$
DELIMITER ;

CALL my_schema.fa_get_sessions_for_im_stations_thesis_im_periods();

ALTER TABLE my_schema.station_im_periods
ADD COLUMN target_start_date VARCHAR(15),
ADD COLUMN target_end_date VARCHAR(15);


UPDATE my_schema.station_im_periods A
JOIN 
	(
		SELECT 
			station_id,
            starting_im_period
        FROM my_schema.station_im_periods 
        WHERE starting_im_period IS NOT NULL
        GROUP BY station_id, starting_im_period
	) B ON A.station_id = B.station_id
		AND A.flowdate >= B.starting_im_period
SET A.target_start_date = B.starting_im_period
WHERE A.single_IM_day IS NULL;

UPDATE my_schema.station_im_periods A
JOIN 
	(
		SELECT 
			station_id,
            end_im_period
        FROM my_schema.station_im_periods 
        WHERE end_im_period IS NOT NULL
        GROUP BY station_id, end_im_period DESC
	) B ON A.station_id = B.station_id
		AND A.flowdate <= B.end_im_period
SET A.target_end_date = B.end_im_period
WHERE A.single_IM_day IS NULL;

### Obtain session information on these stations


DROP TEMPORARY TABLE IF EXISTS my_schema.fa_im_stations_and_sessions_by_day;
CREATE TEMPORARY TABLE my_schema.fa_im_stations_and_sessions_by_day
(
   `flowdate` date NOT NULL,
   `station_id` bigint(20) NOT NULL,
   `station_latitude` decimal(32,4) DEFAULT NULL,   
   `station_longitude` decimal(32,4) DEFAULT NULL,   
   `charger_company_col` varchar(16) DEFAULT NULL,
   `im_date` date DEFAULT NULL,
   `im_start` varchar(21),
   `single_im_day` date DEFAULT NULL,
   `im_end` varchar(24) DEFAULT NULL,
   `minutes_diff` bigint(21) DEFAULT NULL,
   `target_start_date` varchar(15) DEFAULT NULL,
   `target_end_date` varchar(15) DEFAULT NULL,
   `session_start_date` varchar(10) DEFAULT NULL,
   `sessions` bigint(21) DEFAULT 0,
   `minutes` decimal(32,0) DEFAULT NULL,
   `session_kwh` decimal(32,4) DEFAULT NULL,
   `unique_customers` bigint(21) DEFAULT 0
--    `session_start_date_time` mediumtext DEFAULT NULL,
--    `session_end_date_time` mediumtext DEFAULT NULL,
--    `minutes_ordered` mediumtext DEFAULT NULL,
--    `kwh_ordered` mediumtext DEFAULT NULL
 );


DROP PROCEDURE IF EXISTS my_schema.fa_get_sessions_for_im_stations_thesis;
DELIMITER $$
CREATE PROCEDURE my_schema.fa_get_sessions_for_im_stations_thesis()
BEGIN 
	DROP TEMPORARY TABLE IF EXISTS my_schema.station_tracker;
	CREATE TEMPORARY TABLE my_schema.station_tracker
	SELECT station_id FROM my_schema.station_im_periods GROUP BY 1;
    SET @station_id := (SELECT MIN(station_id) FROM my_schema.station_tracker);
    
    WHILE @station_id IS NOT NULL DO
		REPLACE INTO my_schema.fa_im_stations_and_sessions_by_day
		SELECT
			A.flowdate,
			A.station_id,
			A.station_latitude,
			A.station_longitude,
			A.charger_company_col,
			A.im_date, 
			DATE_FORMAT(A.im_start, '%Y-%m-%d %H:%i:%s') AS im_start,
			A.single_im_day,
			DATE_FORMAT(A.im_end, '%Y-%m-%d %H:%i:%s') AS im_end,
			A.minutes_diff,
			A.target_start_date,
			A.target_end_date,
			B.session_start_date,
			B.sessions,
			B.minutes,
			B.session_kwh,
			B.unique_customers
-- 			B.session_start_date_time,
-- 			B.session_end_date_time,
-- 			B.minutes_ordered,
-- 			B.kwh_ordered
		FROM my_schema.station_im_periods A
		LEFT JOIN 
			(
				SELECT
					A.station_id,
-- 					S.station_latitude,
-- 					S.station_longitude,
-- 					GROUP_CONCAT(DISTINCT A.session_start_on ORDER BY A.session_start_on ASC SEPARATOR '; ') AS session_start_date_time,
-- 					GROUP_CONCAT(DISTINCT A.session_stopped_on ORDER BY A.session_stopped_on ASC SEPARATOR '; ') AS session_end_date_time,
					DATE_FORMAT(A.session_start_on, '%Y-%m-%d') AS session_start_date,

-- 					GROUP_CONCAT(DISTINCT A.session_minutes ORDER BY A.session_start_on ASC SEPARATOR '; ') AS minutes_ordered,
-- 					GROUP_CONCAT(DISTINCT A.session_kwh ORDER BY A.session_start_on ASC SEPARATOR '; ') AS kwh_ordered,
					
					COUNT(DISTINCT session_id) AS sessions,
					SUM(A.session_minutes) AS minutes,
					SUM(A.session_kwh) AS session_kwh,
					COUNT(DISTINCT customer_id) AS unique_customers
				FROM my_schema.usage_data A
				JOIN my_schema.station S ON A.station_socket_id = S.station_socket_id
				WHERE A.station_id = @station_id
					AND A.session_start_on BETWEEN '2019-01-01 00:00:00' AND '2020-12-31 23:59:59'
					AND A.host_id NOT IN (215,364,423,429,76)
					AND (A.accepted_flag = 1 AND A.below_max_capacity = 1 AND A.corrupted_flag = 0 AND A.exceed_min_time = 1)
					AND S.station_managed = 1
				GROUP BY A.station_id, session_start_date
			) B ON A.station_id = B.station_id
				AND A.flowdate = B.session_start_date
		WHERE A.station_id = @station_id
			AND A.flowdate BETWEEN '2019-01-01' AND LAST_DAY('2020-12-01')
		ORDER BY A.station_id, A.flowdate;
        DELETE FROM my_schema.station_tracker WHERE station_id = @station_id;
		SET @station_id := (SELECT MIN(station_id) FROM my_schema.station_tracker);
	END WHILE;
END $$
DELIMITER ;

CALL my_schema.fa_get_sessions_for_im_stations_thesis();

DROP TEMPORARY TABLES IF EXISTS my_schema.la_jan_daily;
DROP TEMPORARY TABLES IF EXISTS my_schema.final_IM_minutes_both_connectors;
DROP TEMPORARY TABLES IF EXISTS my_schema.one_IM_day;
DROP TEMPORARY TABLES IF EXISTS my_schema.same_IM_start_end_time;
DROP TEMPORARY TABLES IF EXISTS my_schema.other_ims;
DROP TEMPORARY TABLES IF EXISTS my_schema.other_minutes;
DROP TEMPORARY TABLES IF EXISTS my_schema.scenarios_236;
DROP TEMPORARY TABLES IF EXISTS my_schema.IM_minutes_by_day_station;
DROP TEMPORARY TABLES IF EXISTS my_schema.station_im_periods;
DROP TEMPORARY TABLES IF EXISTS my_schema.station_tracker;



SELECT *
FROM my_schema.fa_im_stations_and_sessions_by_day
;


stop.code;




-- SELECT * FROM my_schema.la_jan_daily LIMIT 100; # by station_id, station_socket_id, day (all days in Jan 2021)
-- SELECT * FROM my_schema.IM_minutes_by_day_station WHERE station_id = 279 ; # by station_id and day (only includes the days the station was completely out.. includes in_maintenance_minutes)
-- SELECT * FROM my_schema.final_IM_minutes_both_connectors LIMIT 1000; # by station_id and day (only includes the days the station was completely out.. includes in_maintenance_minutes and im_start and im_end when it went out)

############
# Extract this data
######

SELECT *
FROM my_schema.fa_im_stations_and_sessions_by_day A
JOIN (
		SELECT 
			station_id,
            station_latitude,
            station_longitude,
            station_city,
            station_state,
            station_zip
        FROM my_schema.station
        GROUP BY station_id
	) B ON A.station_id = B.station_id
;

SELECT * FROM my_schema.station_tracker;
SELECT station_id FROM my_schema.station_im_periods GROUP BY 1;
-- SELECT * FROM my_schema.usage_data LIMIT 100;


####################################################

# all im day summaries
SELECT 
	COALESCE(target_start_date, single_im_day, 'OTHER') AS stages,
	COUNT(DISTINCT im_date) AS im_days
FROM my_schema.fa_im_stations_and_sessions_by_day
WHERE station_id = 6780
GROUP BY stages;

### non im day summaries
# first_clean_slate if applies
SELECT 
	'Initial Non-IM days' AS stage,
    COUNT(DISTINCT A.flowdate) AS days
FROM my_schema.fa_im_stations_and_sessions_by_day A
-- JOIN my_schema.fa_im_stations_and_sessions_by_day B ON A.station_id = B.station_id
-- 	AND A.flowdate < B.single_im_day
WHERE A.station_id = 6780
	AND A.im_date IS NULL
    AND A.target_start_date IS NULL
;
# all other non-IM days
SELECT
	CASE
		WHEN A.flowdate BETWEEN A.target_start_date AND IFNULL(A.target_end_date, '2020-12-31') THEN ADDDATE(A.target_start_date, INTERVAL (B.im_days) DAY)
	END AS stage,
    COUNT(DISTINCT A.flowdate) AS days
FROM my_schema.fa_im_stations_and_sessions_by_day A
LEFT JOIN
	 (
		SELECT 
			COALESCE(target_start_date, single_im_day, 'OTHER') AS stages,
			COUNT(DISTINCT im_date) AS im_days
		FROM my_schema.fa_im_stations_and_sessions_by_day
		WHERE station_id = 6780
		GROUP BY stages
     ) B ON IFNULL(A.target_start_date, A.single_im_day) = B.stages
WHERE A.station_id = 6780
	AND A.im_date IS NULL
--     AND A.target_start_date IS NOT NULL
GROUP BY stage
ORDER BY A.flowdate;

####################################################

SELECT *
FROM my_schema.usage_data
WHERE station_id = 43694
	AND session_start_on BETWEEN '2020-05-01 00:00:00' AND '2020-05-31 23:59:59'
;




SELECT 
	X.station_id,
    COUNT(DISTINCT IF(X.flowdate < first_im, flowdate, NULL)) AS days_before_im,
    COUNT(DISTINCT IF(first_im <= im_end, flowdate, NULL)) AS first_days_im
FROM 
	(
		SELECT 
			A.*,
            first_im.first_im,
			B.date,
            B.im_start,
            B.im_end,
            B.minutes_diff,
			S.station_socket_type,
			S.station_maximum_power,
			S.station_latitude,
			S.station_longitude,
			S.station_address,
			S.station_city,
			S.station_state,
			S.station_zip
		FROM my_schema.la_jan_daily A
		JOIN (SELECT S.* FROM my_schema.station S JOIN my_schema.dma DMA ON DMA.zip = S.station_zip AND DMA.active = 1 GROUP BY station_id) S ON S.station_id = A.station_id # get information about the stations from edw.station
		LEFT JOIN my_schema.final_IM_minutes_both_connectors B ON A.flowdate = B.date # add info on days in maintenance
			AND A.station_id = B.station_id
		LEFT JOIN 
			(
				SELECT 
					station_id,
					MIN(date) AS first_im
				FROM my_schema.final_IM_minutes_both_connectors
				GROUP BY station_id
			) first_im ON first_im.station_id = A.station_id
		LEFT JOIN 
			(
				SELECT 
					station_id,
					MIN(date) AS first_im
				FROM my_schema.final_IM_minutes_both_connectors
				GROUP BY station_id
			) second_im ON first_im.station_id = A.station_id
		WHERE A.station_id IN (SELECT DISTINCT station_id FROM my_schema.final_IM_minutes_both_connectors WHERE minutes_diff > 300) # limit to stations that was fully in maintenance for at least 5 hours
			AND A.station_id = 582
		GROUP BY A.station_id, A.flowdate
		ORDER BY A.station_id, A.flowdate
    ) X
GROUP BY X.station_id
;

ALTER TABLE my_schema.fa_im_stations_and_sessions_by_day
ADD INDEX(station_id, flowdate),
ADD INDEX(station_id, im_date);



stop.code;




# Archived on September 13, 2021
-- DROP PROCEDURE IF EXISTS my_schema.fa_both_connectors_IM_ANALYSIS;
-- DELIMITER $$
-- CREATE PROCEDURE my_schema.fa_both_connectors_IM_ANALYSIS(IN day1 DATE, day2 DATE)
-- BEGIN 
--  	DROP TEMPORARY TABLE IF EXISTS my_schema.BOTH_OUT_BASE_TABLE;
--  	CREATE TEMPORARY TABLE my_schema.BOTH_OUT_BASE_TABLE
--  	(
--  	   `date` DATE DEFAULT NULL,
--  	   `station_id` bigint(20) DEFAULT NULL,
--  	   `im_start` varchar(21) DEFAULT NULL,
--  	   `im_end` varchar(24) DEFAULT NULL,
--  	   `minutes_diff` bigint(21) DEFAULT NULL
--  	);
--  	SET @isdate := day1;
-- 	SET @enddate := day2;
--      
-- 	WHILE @isdate <= @enddate DO
--  
--  		DROP TEMPORARY TABLE IF EXISTS my_schema.one_IM_day;
--  		CREATE TEMPORARY TABLE my_schema.one_IM_day
--  		SELECT
--  			IM.ref_date,
--  			IM.station_id,
--  			IM.station_socket_id,
--  			DATE_FORMAT(IM.in_maintenance_start, '%Y-%m-%d %H:%i') AS in_maintenance_start,
--  			DATE_FORMAT(IM.adjusted_in_maintenance_start, '%Y-%m-%d %H:%i') AS adjusted_in_maintenance_start,
--  			DATE_FORMAT(IM.in_maintenance_end, '%Y-%m-%d %H:%i') AS in_maintenance_end,
--  			DATE_FORMAT(IM.adjusted_in_maintenance_end, '%Y-%m-%d %H:%i') AS adjusted_in_maintenance_end,
--  			CASE 
--  				WHEN (S.station_model LIKE 'ABB %' AND S.station_model NOT IN ('ABB Terra 53 50kW|CCS|Maven', 'ABB Terra 54HV 50kW|CCS|Penske'))	THEN 'ABB'
--  				WHEN S.station_model IN ('ABB Terra 53 50kW|CCS|Maven') 																			THEN 'Maven Dedicated'
--  				WHEN S.station_model IN ('ABB Terra 54HV 50kW|CCS|Penske') 																			THEN 'Penske Dedicated'
--  				WHEN S.station_model LIKE 'BTC %' 																									THEN 'BTC'
--  				WHEN S.station_model IN ('Efacec 50kW|CHAdeMO CCS') 																				THEN 'EFACEC'
--  				WHEN S.station_model IN ('Bosch SPX PowerXpress L2|30A') 																			THEN 'SPX'
--  				WHEN S.station_model IN ('DCQC 44kW|CHAdeMO') 																						THEN 'NISSAN'
--  				WHEN S.station_model IN ('Sema ChargePro 620 L2|30A') 																				THEN 'SEMA'
--  				WHEN S.station_model IN ('DELTA AC Mini L2|40A') 																					THEN 'Delta'
--  				WHEN S.station_model IN ('LiteOn IC3 L2|32A' , 'LiteOn L2|30A Non-networked') 														THEN 'LiteOn'
--  				WHEN S.station_model IN ('Aero EVSE-CS L2|30A') 																					THEN 'Aerovironment'
--  			END AS charger_company_col,
--              IM.in_maintenance_minutes
--  		FROM my_schema.in_maintenance IM
--  		JOIN my_schema.station S ON S.station_socket_id = IM.station_socket_id
--  		WHERE IM.ref_date = @isdate
-- 			AND IM.in_maintenance_minutes > 0
-- 			AND S.station_socket_type = 'DC'
-- 			AND S.host_id NOT IN (215,364,423,429,76)
-- 		;
--  		
-- 		UPDATE my_schema.one_IM_day IM
--  		JOIN my_schema.station_maintenance_exclusion SME ON IM.station_socket_id = SME.station_socket_id 
--  			AND IM.ref_date = SME.ref_date
--  		SET IM.in_maintenance_minutes = 0 
--  		WHERE SME.Internal_External = 'E';

-- 		UPDATE my_schema.one_IM_day IM
-- 		SET in_maintenance_minutes = 0
-- 		WHERE IM.in_maintenance_minutes > 0
-- 			AND NOT EXISTS (SELECT *
-- 						FROM my_schema.station_maint_issues A 
-- 						WHERE A.station_id = IM.station_id 
-- 							AND IM.ref_date BETWEEN A.created_date AND A.status_closed_date);
-- 		
--         # Scenario 1
--  		DROP TEMPORARY TABLE IF EXISTS my_schema.same_IM_start_end_time;
--  		CREATE TEMPORARY TABLE my_schema.same_IM_start_end_time
--  		SELECT 
--  			IM.ref_date,
--  			IM.station_id,
--  			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS in_maintenance_start_min,
--  			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS in_maintenance_end_min,
--  			COUNT(DISTINCT IM.station_socket_id) AS station_sockets
--  		FROM my_schema.one_IM_day IM 
--  		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
--         WHERE in_maintenance_minutes != 0 
--  		GROUP BY in_maintenance_start_min, in_maintenance_end_min, station_id
--  		HAVING station_sockets > 1;
-- 		
--         # Identify all other NON-scenario 1s
--  		DROP TEMPORARY TABLE IF EXISTS my_schema.other_ims;
--  		CREATE TEMPORARY TABLE my_schema.other_ims
--  		SELECT
--  			A.ref_date,
--  			A.station_id
--  			,COUNT(DISTINCT station_socket_id) AS connectors
--  		FROM my_schema.one_IM_day A
--  		LEFT JOIN my_schema.same_IM_start_end_time B ON A.station_id = B.station_id
--  			AND IFNULL(A.adjusted_in_maintenance_start, A.in_maintenance_start) = B.in_maintenance_start_min
--  			AND IFNULL(A.adjusted_in_maintenance_end, A.in_maintenance_end) = B.in_maintenance_end_min
--  		WHERE B.station_id IS NULL
--  		GROUP BY station_id, A.ref_date
--  		HAVING connectors > 1;
--  
--  		## Scenario # 4
--  		DROP TEMPORARY TABLE IF EXISTS my_schema.other_minutes;
--  		CREATE TEMPORARY TABLE my_schema.other_minutes
--  		SELECT
--  			IM.station_id,
--  			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
--  			MIN(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s')) AS im_end,
--  			TIMESTAMPDIFF(MINUTE, DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s'), MIN(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s'))) AS minutes_diff
--  
--  		FROM my_schema.in_maintenance IM
--  		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
--  		JOIN 
--  			(
--  				## collects scenario 4
--  				SELECT
--  					IM.ref_date,
--  					IM.station_id,
--  					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
--  					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
--  					COUNT(*) AS counts
--  				FROM my_schema.one_IM_day IM
--  				JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
--  				JOIN my_schema.other_ims OI ON OI.ref_date = IM.ref_date
--  					AND OI.station_id = IM.station_id
--  				WHERE IM.in_maintenance_minutes > 0
-- 					AND S.host_id NOT IN (215,364,423,429,76)
--  				GROUP BY DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i'), IM.station_id
--  				HAVING counts > 1
--  			) X ON IM.station_id = X.station_id
--  					AND IM.ref_date = X.ref_date
--  					AND DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') = X.im_start
--  		GROUP BY IM.station_id;
--  
--  		## Scenario # 5
--  		INSERT INTO my_schema.other_minutes
--  		SELECT
-- 			S.station_id,
-- 			MAX(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s')) AS im_start,
-- 			DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
-- 			TIMESTAMPDIFF(MINUTE, MAX(DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i:%s')), DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i:%s')) AS minutes_diff
--  
--  		FROM my_schema.in_maintenance IM
--  		JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
--  		JOIN
--  			(
--  				## collects scenario 5
--  				SELECT
--  					IM.ref_date,
--  					IM.station_id,
--  					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i') AS im_start,
--  					DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') AS im_end,
--  					COUNT(*) AS counts
--  				FROM my_schema.one_IM_day IM
--  				JOIN my_schema.station S ON IM.station_socket_id = S.station_socket_id
--  				JOIN my_schema.other_ims OI ON OI.ref_date = IM.ref_date
--  					AND OI.station_id = IM.station_id
--  				LEFT JOIN my_schema.other_minutes OM ON OM.station_id = IM.station_id
--  					AND OM.im_start = DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start), '%Y-%m-%d %H:%i')
--  				WHERE OM.station_id IS NULL
--  					AND IM.in_maintenance_minutes > 0
-- 					AND S.host_id NOT IN (215,364,423,429,76)
--  				GROUP BY DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i'), IM.station_id
--  				HAVING counts > 1
--  			) X ON IM.station_id = X.station_id
--  					AND IM.ref_date = X.ref_date
--  					AND DATE_FORMAT(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end), '%Y-%m-%d %H:%i') = X.im_end
--  		GROUP BY IM.station_id;
--  
-- 		# Covers scenarios 2, 3, 6: connectors w/ diff im-start and im-end and have some overlap
-- 		DROP TEMPORARY TABLE IF EXISTS my_schema.scenarios_236;
-- 		CREATE TEMPORARY TABLE my_schema.scenarios_236
-- 		SELECT 
-- 			IM.ref_date,
-- 			IM.station_id,
-- 			1440 - IFNULL(IF(TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) = 0, 1440, 
-- 					TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)))), 0) AS x_0,
-- 			IFNULL(TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), DATE_FORMAT(MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), '%Y-%m-%d 00:00')), 0) AS x_1,
-- 			
-- 			TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) AS a,
-- 			TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), MAX(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start))) AS b,
-- 			
-- 			1440 - (1440 - IFNULL(IF(TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) = 0, 1440, 
-- 					TIMESTAMPDIFF(MINUTE, DATE_FORMAT(MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), '%Y-%m-%d 00:00'), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)))), 0) +
-- 					IFNULL(TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), DATE_FORMAT(MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), '%Y-%m-%d 00:00')), 0) +
-- 					TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end)), MAX(IFNULL(IM.adjusted_in_maintenance_end, IM.in_maintenance_end))) +
-- 					TIMESTAMPDIFF(MINUTE, MIN(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)), MAX(IFNULL(IM.adjusted_in_maintenance_start, IM.in_maintenance_start)))) AS overlapping_minutes_both_connectors_IM
-- 		FROM my_schema.one_IM_day IM
-- 		JOIN (
-- 				SELECT A.*
-- 				FROM my_schema.other_ims A
-- 				LEFT JOIN my_schema.other_minutes B ON A.station_id = B.station_id
-- 				WHERE B.station_id IS NULL
-- 			) OT ON OT.ref_date = IM.ref_date
-- 				AND OT.station_id = IM.station_id
-- 		WHERE IM.in_maintenance_minutes != 0
-- 		GROUP BY IM.ref_date, IM.station_id;


--  		INSERT INTO my_schema.BOTH_OUT_BASE_TABLE
--         # both connectors start/end IM
--  		SELECT
--  			@isdate AS date,
--  			station_id,
--  			in_maintenance_start_min AS im_start,
--  			in_maintenance_end_min AS im_end,
--  			TIMESTAMPDIFF(MINUTE, in_maintenance_start_min, in_maintenance_end_min) AS minutes_diff
--  		FROM my_schema.same_IM_start_end_time
--  		UNION ALL
--         # both connectors start IM or end IM at the same time
--  		SELECT
--  			@isdate AS date, 
--  			A.*
-- 		FROM my_schema.other_minutes A
--         UNION ALL
-- 		# each connector has different start IM and different end IM 
--         SELECT 
-- 			@isdate AS date,
-- 			station_id,
-- 			ref_date AS im_start, # doesn't matter
-- 			ref_date AS im_end, # doesn't matter
-- 			SUM(overlapping_minutes_both_connectors_IM) AS minutes_diff
-- 		FROM my_schema.scenarios_236
-- 		GROUP BY station_id;
--         
-- 		SET @isdate = ADDDATE(@isdate, INTERVAL 1 DAY);
-- 	END WHILE;
--      
-- 	DELETE FROM my_schema.BOTH_OUT_BASE_TABLE
-- 	WHERE station_id IS NULL;
-- 	
--     SELECT *
--     FROM my_schema.BOTH_OUT_BASE_TABLE
--     ;
-- -- 	DROP TEMPORARY TABLE IF EXISTS my_schema.IM_minutes_by_day_station;
-- -- 	CREATE TEMPORARY TABLE my_schema.IM_minutes_by_day_station
-- -- 	SELECT 
-- -- 		date,
-- -- 		station_id,
-- -- 		SUM(minutes_diff) AS IM_minutes,
-- -- 		1440 AS possible_minutes,
-- -- 		1440 - SUM(minutes_diff) AS NON_IM_minutes,
-- -- 		ROUND(SUM(minutes_diff / 60), 2) AS IM_hours,
-- -- 		24 AS possible_hours,
-- -- 		ROUND(24 - SUM(minutes_diff / 60), 2) AS NON_IM_hours
-- -- 	FROM my_schema.BOTH_OUT_BASE_TABLE
-- -- 	GROUP BY date, station_id;
-- --      
-- -- 	SELECT 
-- -- 		SUM(IM_minutes) AS im_minutes,
-- -- 		SUM(possible_minutes) AS possible_minutes,
-- -- 		SUM(possible_hours) AS possible_hours,
-- -- 		SUM(IM_hours) AS IM_hours,
-- -- 		(SUM(possible_minutes) - SUM(IM_minutes)) / SUM(possible_minutes) AS uptime,
-- -- 		COUNT(DISTINCT station_id) AS unique_stations,
-- -- 		ROUND((SUM(IM_minutes) / COUNT(DISTINCT station_id)) / COUNT(DISTINCT date), 2) AS avg_IM_minutes_per_station_per_day, 
-- -- 		ROUND((SUM(IM_hours) / COUNT(DISTINCT station_id)) / COUNT(DISTINCT date), 2) AS avg_IM_hours_per_station_per_day,

-- -- 		SUM(NON_IM_minutes) AS non_im_minutes,
-- -- 		SUM(NON_IM_hours) AS non_im_hours,
-- -- 		ROUND((SUM(NON_IM_minutes) / COUNT(DISTINCT station_id)) / COUNT(DISTINCT date), 2) AS avg_NON_IM_minutes_per_station_per_day, 
-- -- 		ROUND((SUM(NON_IM_hours) / COUNT(DISTINCT station_id)) / COUNT(DISTINCT date), 2) AS avg_NON_IM_hours_per_station_per_day 
-- -- 	FROM my_schema.IM_minutes_by_day_station;

-- END $$
-- DELIMITER ;



-- CALL my_schema.fa_both_connectors_IM_ANALYSIS('2021-08-01', '2021-08-20');




-- SELECT *
-- FROM my_schema.BOTH_OUT_BASE_TABLE
-- -- WHERE station_id = 287
-- -- 	AND date = '2021-08-04'
-- ORDER BY station_id, date ASC
-- ;


-- -- 2021-08-04	287	2021-08-04 07:52	2021-08-04 12:54	302
-- -- 2021-08-04	287	2021-08-04 13:14	2021-08-05 00:00	646

-- SELECT *
-- FROM my_schema.in_maintenance
-- WHERE station_id = 287
-- 	AND station_socket_id != 0
-- 	AND ref_date = '2021-08-04'
-- ;






-- SELECT
-- 	BT.station_id,
--     COUNT(DISTINCT IF(BT.minutes_diff = 1440 AND BT.date = DATE(DM.flowdate), BT.date, NULL)) AS full_days_IM,
--     COUNT(DISTINCT IF(BT.minutes_diff <> 1440 AND BT.date = DATE(DM.flowdate), BT.date, NULL)) AS partial_days_IM,
--     COUNT(DISTINCT DM.flowdate) - COUNT(DISTINCT BT.date) AS non_days_in_IM
-- FROM my_schema2.date_matrix DM
-- LEFT JOIN my_schema.BOTH_OUT_BASE_TABLE BT ON 1=1
-- WHERE DM.flowdate BETWEEN '2021-08-01' AND DATE(SUBDATE(SUBDATE(NOW(), INTERVAL 7 HOUR), INTERVAL 1 DAY))
-- 	AND station_id = 5935
-- GROUP BY BT.station_id
-- ;



-- SELECT *
-- FROM my_schema.BOTH_OUT_BASE_TABLE
-- WHERE station_id = 5935
-- ;




-- SELECT 
-- 	DM.*, 
--     BT.*
-- FROM my_schema2.date_matrix DM
-- LEFT JOIN my_schema.BOTH_OUT_BASE_TABLE BT ON 1=1
-- WHERE DM.flowdate BETWEEN '2021-08-01' AND DATE(SUBDATE(SUBDATE(NOW(), INTERVAL 7 HOUR), INTERVAL 1 DAY))
-- 	AND BT.station_id = 5935
-- ORDER BY BT.station_id, BT.date, DM.flowdate
-- ;



-- SELECT session_start_on, A.*
-- FROM my_schema.usage_data A
-- WHERE station_id = 5935
-- 	AND session_start_on >= '2021-08-01'
-- ORDER BY session_start_on 