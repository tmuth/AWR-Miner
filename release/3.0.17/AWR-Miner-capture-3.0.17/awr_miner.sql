set define '&'
set concat '~'
set pagesize 50000
REPHEADER OFF
REPFOOTER OFF

set serveroutput on
set verify off
column cnt_dbid_1 new_value CNT_DBID noprint

define NUM_DAYS = 30
define SQL_TOP_N = 30
define AWR_MINER_VER = 3.0.17

alter session set cursor_sharing = exact;


prompt 
prompt This script queries views in the AWR Repository that require 
prompt a license for the Diagnostic Pack. These are the same views used
prompt to generate an AWR report.
prompt If you are licensed for the Diagnostic Pack please type YES. 
prompt Otherwise please type NO and this script will exit.
define DIAG_PACK_LICENSE = 'NO'
prompt
accept DIAG_PACK_LICENSE CHAR prompt 'Are you licensed for the Diagnostic Pack? [NO|YES] ' 



whenever sqlerror exit
set serveroutput on
begin
    if upper('&DIAG_PACK_LICENSE') = 'YES' then
		null;
	else
        dbms_output.put_line('This script will now exit.');
        execute immediate 'bogus statement to force exit';
    end if;
end;
/

whenever sqlerror continue

SELECT count(DISTINCT dbid) cnt_dbid_1
FROM dba_hist_database_instance;
 --where rownum = 1;


define DBID = ' ' 
column :DBID_1 new_value DBID noprint
variable DBID_1 varchar2(30)

define DB_VERSION = 0
column :DB_VERSION_1 new_value DB_VERSION noprint
variable DB_VERSION_1 number



set feedback off
declare
	version_gte_11_2	varchar2(30);
	l_sql				varchar2(32767);
	l_variables	        varchar2(1000) := ' ';
	l_block_size		number;
begin
	:DB_VERSION_1 :=  dbms_db_version.version + (dbms_db_version.release / 10);
	dbms_output.put_line('Database IDs in this Repository:');
	
	
	
	for c1 in (select distinct dbid,db_name FROM dba_hist_database_instance order by db_name)
	loop
		dbms_output.put_line(rpad(c1.dbid,35)||c1.db_name);
	end loop; --c1
		
	if to_number(&CNT_DBID) > 1 then
		:DBID_1 := ' ';
	else
		
		SELECT DISTINCT dbid into :DBID_1
					 FROM dba_hist_database_instance
					where rownum = 1;
		

	end if;
	
	--l_variables := l_variables||'ver_gte_11_2:TRUE';
	
	if :DB_VERSION_1  >= 11.2 then
		l_variables := l_variables||'ver_gte_11_2:TRUE';
	else
		l_variables := l_variables||'ver_gte_11_2:FALSE';
	end if;
	
	if :DB_VERSION_1  >= 11.1 then
		l_variables := l_variables||',ver_gte_11_1:TRUE';
	else
		l_variables := l_variables||',ver_gte_11_1:FALSE';
	end if;
	
	--alter session set plsql_ccflags = 'debug_flag:true';
	l_sql := q'[alter session set plsql_ccflags =']'||l_variables||q'[']';
	
	
	
	execute immediate l_sql;
end;
/

select :DBID_1 from dual;
select :DB_VERSION_1 from dual;



accept DBID2 CHAR prompt 'Which dbid would you like to use? [&DBID] '

column DBID_2 new_value DBID noprint
select case when length('&DBID2') > 3 then '&DBID2' else '&DBID' end DBID_2 from dual;

REM set heading off

select '&DBID' a from dual;

column db_name1 new_value DBNAME
prompt Will export AWR data for the following Database:

SELECT dbid,db_name db_name1
FROM dba_hist_database_instance
where dbid = '&DBID'
and rownum = 1;


define T_WAITED_MICRO_COL = 'TIME_WAITED_MICRO' 
column :T_WAITED_MICRO_COL_1 new_value T_WAITED_MICRO_COL noprint
variable T_WAITED_MICRO_COL_1 varchar2(30)

begin
	if :DB_VERSION_1  >= 11.1 then
		:T_WAITED_MICRO_COL_1 := 'TIME_WAITED_MICRO_FG';
	else
		:T_WAITED_MICRO_COL_1 := 'TIME_WAITED_MICRO';
	end if;

end;
/

select :T_WAITED_MICRO_COL_1 from dual;

column DB_BLOCK_SIZE_1 new_value DB_BLOCK_SIZE noprint
SELECT VALUE DB_BLOCK_SIZE_1
	FROM DBA_HIST_PARAMETER
	WHERE dbid = &DBID
	and PARAMETER_NAME = 'db_block_size'
	AND snap_id = (SELECT MAX(snap_id) FROM dba_hist_osstat WHERE dbid = &DBID AND instance_number = 1)
   AND instance_number = 1;

column snap_min1 new_value SNAP_ID_MIN noprint
SELECT min(snap_id) - 1 snap_min1
  FROM dba_hist_snapshot
  WHERE dbid = &DBID 
    and begin_interval_time > (
		SELECT max(begin_interval_time) - &NUM_DAYS
		  FROM dba_hist_snapshot 
		  where dbid = &DBID);
		  
column snap_max1 new_value SNAP_ID_MAX noprint
SELECT max(snap_id) snap_max1
  FROM dba_hist_snapshot
  WHERE dbid = &DBID;
  
column FILE_NAME new_value SPOOL_FILE_NAME noprint
select 'awr-hist-'||'&DBID'||'-'||'&DBNAME'||'-'||ltrim('&SNAP_ID_MIN')||'-'||ltrim('&SNAP_ID_MAX')||'.out' FILE_NAME from dual;
spool &SPOOL_FILE_NAME

-- ##############################################################################################
REPHEADER ON
REPFOOTER ON 

set linesize 1000 
set numwidth 10
set wrap off
set heading on
set trimspool on
set feedback off




set serveroutput on
DECLARE
    l_pad_length number :=60;
	l_hosts	varchar2(4000);
BEGIN


    dbms_output.put_line('~~BEGIN-OS-INFORMATION~~');
    dbms_output.put_line(rpad('STAT_NAME',l_pad_length)||' '||'STAT_VALUE');
    dbms_output.put_line(rpad('-',l_pad_length,'-')||' '||rpad('-',l_pad_length,'-'));
    
    FOR c1 IN (SELECT 
                      CASE WHEN stat_name = 'PHYSICAL_MEMORY_BYTES' THEN 'PHYSICAL_MEMORY_GB' ELSE stat_name END stat_name,
                      CASE WHEN stat_name IN ('PHYSICAL_MEMORY_BYTES') THEN round(VALUE/1024/1024/1024,2) ELSE VALUE END stat_value
                  FROM dba_hist_osstat 
                 WHERE dbid = &DBID 
                   AND snap_id = (SELECT MAX(snap_id) FROM dba_hist_osstat WHERE dbid = &DBID  and instance_number = 1)
                   AND instance_number = 1
                   AND (stat_name LIKE 'NUM_CPU%'
                   OR stat_name IN ('PHYSICAL_MEMORY_BYTES')))
    loop
        dbms_output.put_line(rpad(c1.stat_name,l_pad_length)||' '||c1.stat_value);
    end loop; --c1
    
    
	FOR c2 IN (SELECT 
						$IF $$VER_GTE_11_2 $THEN
							REPLACE(platform_name,' ','_') platform_name,
						$ELSE
							'None' platform_name,
						$END
						VERSION,db_name FROM dba_hist_database_instance 
						WHERE dbid = &DBID  
						and startup_time = (select max(startup_time) from dba_hist_database_instance WHERE dbid = &DBID )
						AND ROWNUM = 1)
    loop
        dbms_output.put_line(rpad('PLATFORM_NAME',l_pad_length)||' '||c2.platform_name);
        dbms_output.put_line(rpad('VERSION',l_pad_length)||' '||c2.VERSION);
        dbms_output.put_line(rpad('DB_NAME',l_pad_length)||' '||c2.db_name);
    end loop; --c2
    
    FOR c3 IN (SELECT count(distinct s.instance_number) instances 
			     FROM dba_hist_database_instance i,dba_hist_snapshot s
				WHERE i.dbid = s.dbid
				  and i.dbid = &DBID
				  AND s.snap_id BETWEEN &SNAP_ID_MIN AND &SNAP_ID_MAX)
    loop
        dbms_output.put_line(rpad('INSTANCES',l_pad_length)||' '||c3.instances);
    end loop; --c3           
	
	
	FOR c4 IN (SELECT distinct regexp_replace(host_name,'^([[:alnum:]]+)\..*$','\1')  host_name 
			     FROM dba_hist_database_instance i,dba_hist_snapshot s
				WHERE i.dbid = s.dbid
				  and i.dbid = &DBID
                  and s.startup_time = i.startup_time
				  AND s.snap_id BETWEEN &SNAP_ID_MIN AND &SNAP_ID_MAX
			    order by 1)
    loop
		l_hosts := l_hosts || c4.host_name ||',';
	end loop; --c4
	l_hosts := rtrim(l_hosts,',');
	dbms_output.put_line(rpad('HOSTS',l_pad_length)||' '||l_hosts);
	
	dbms_output.put_line(rpad('AWR_MINER_VER',l_pad_length)||' &AWR_MINER_VER');
	dbms_output.put_line('~~END-OS-INFORMATION~~');
END;
/

prompt 
prompt 


-- ##############################################################################################

REPHEADER PAGE LEFT '~~BEGIN-MEMORY~~'
REPFOOTER PAGE LEFT '~~END-MEMORY~~'

SELECT snap_id,
    instance_number,
    MAX (DECODE (stat_name, 'SGA', stat_value, NULL)) "SGA",
    MAX (DECODE (stat_name, 'PGA', stat_value, NULL)) "PGA",
    MAX (DECODE (stat_name, 'SGA', stat_value, NULL)) + MAX (DECODE (stat_name, 'PGA', stat_value,
    NULL)) "TOTAL"
   FROM
    (SELECT snap_id,
        instance_number,
        ROUND (SUM (bytes) / 1024 / 1024 / 1024, 1) stat_value,
        MAX ('SGA') stat_name
       FROM dba_hist_sgastat
      WHERE dbid = &DBID
        AND snap_id BETWEEN &SNAP_ID_MIN AND &SNAP_ID_MAX
   GROUP BY snap_id,
        instance_number
  UNION ALL
     SELECT snap_id,
        instance_number,
        ROUND (value / 1024 / 1024 / 1024, 1) stat_value,
        'PGA' stat_name
       FROM dba_hist_pgastat
      WHERE dbid = &DBID
        AND snap_id BETWEEN &SNAP_ID_MIN AND &SNAP_ID_MAX
        AND NAME = 'total PGA allocated'
    )
GROUP BY snap_id,
    instance_number
ORDER BY snap_id,
    instance_number;

prompt 
prompt 

-- ##############################################################################################

 
REPHEADER PAGE LEFT '~~BEGIN-SIZE-ON-DISK~~'
REPFOOTER PAGE LEFT '~~END-SIZE-ON-DISK~~'
 WITH ts_info as (
select dbid, ts#, tsname, max(block_size) block_size
from dba_hist_datafile
where dbid = &DBID
group by dbid, ts#, tsname),
-- Get the maximum snaphsot id for each day from dba_hist_snapshot
snap_info as (
select dbid,to_char(trunc(end_interval_time,'DD'),'MM/DD/YY') dd, max(s.snap_id) snap_id
FROM dba_hist_snapshot s
where s.snap_id between &SNAP_ID_MIN and &SNAP_ID_MAX
and dbid = &DBID
--where s.end_interval_time > to_date(:start_time,'MMDDYYYY')
--and s.end_interval_time < to_date(:end_time,'MMDDYYYY')
group by dbid,trunc(end_interval_time,'DD'))
-- Sum up the sizes of all the tablespaces for the last snapshot of each day
select s.snap_id, round(sum(tablespace_size*f.block_size)/1024/1024/1024,2) size_gb
from dba_hist_tbspc_space_usage sp,
ts_info f,
snap_info s
WHERE s.dbid = sp.dbid
AND s.dbid = &DBID
 and s.snap_id between &SNAP_ID_MIN and &SNAP_ID_MAX
and s.snap_id = sp.snap_id
and sp.dbid = f.dbid
AND sp.tablespace_id = f.ts#
GROUP BY  s.snap_id,s.dd, s.dbid
order by  s.snap_id;

prompt 
prompt   
-- ##############################################################################################


REPHEADER PAGE LEFT '~~BEGIN-MAIN-METRICS~~'
REPFOOTER PAGE LEFT '~~END-MAIN-METRICS~~'

 select snap_id "snap",num_interval "dur_m", end_time "end",inst "inst",
  max(decode(metric_name,'Host CPU Utilization (%)',					average,null)) "os_cpu",
  max(decode(metric_name,'Host CPU Utilization (%)',					maxval,null)) "os_cpu_max",
  max(decode(metric_name,'Database Wait Time Ratio',                   round(average,1),null)) "db_wait_ratio",
max(decode(metric_name,'Database CPU Time Ratio',                   round(average,1),null)) "db_cpu_ratio",
max(decode(metric_name,'CPU Usage Per Sec',                   round(average/100,3),null)) "cpu_per_s",
max(decode(metric_name,'CPU Usage Per Sec',                   round(maxval/100,3),null)) "cpu_per_s_max",
max(decode(metric_name,'Average Active Sessions',                   average,null)) "aas",
max(decode(metric_name,'Average Active Sessions',                   maxval,null)) "aas_max",
max(decode(metric_name,'SQL Service Response Time',                   average,null)) "sql_res_t_cs",
max(decode(metric_name,'Background Time Per Sec',                   average,null)) "bkgd_t_per_s",
max(decode(metric_name,'Logons Per Sec',                            average,null)) "logons_s",
max(decode(metric_name,'Current Logons Count',                      average,null)) "logons_total",
max(decode(metric_name,'Executions Per Sec',                        average,null)) "exec_s",
max(decode(metric_name,'Hard Parse Count Per Sec',                  average,null)) "hard_p_s",
max(decode(metric_name,'Logical Reads Per Sec',                     average,null)) "l_reads_s",
max(decode(metric_name,'User Commits Per Sec',                      average,null)) "commits_s",
max(decode(metric_name,'Physical Read Total Bytes Per Sec',         round((average)/1024/1024,1),null)) "read_mb_s",
max(decode(metric_name,'Physical Read Total Bytes Per Sec',         round((maxval)/1024/1024,1),null)) "read_mb_s_max",
max(decode(metric_name,'Physical Read Total IO Requests Per Sec',   average,null)) "read_iops",
max(decode(metric_name,'Physical Read Total IO Requests Per Sec',   maxval,null)) "read_iops_max",
max(decode(metric_name,'Physical Reads Per Sec',  			average,null)) "read_bks",
max(decode(metric_name,'Physical Reads Direct Per Sec',  			average,null)) "read_bks_direct",
max(decode(metric_name,'Physical Write Total Bytes Per Sec',        round((average)/1024/1024,1),null)) "write_mb_s",
max(decode(metric_name,'Physical Write Total Bytes Per Sec',        round((maxval)/1024/1024,1),null)) "write_mb_s_max",
max(decode(metric_name,'Physical Write Total IO Requests Per Sec',  average,null)) "write_iops",
max(decode(metric_name,'Physical Write Total IO Requests Per Sec',  maxval,null)) "write_iops_max",
max(decode(metric_name,'Physical Writes Per Sec',  			average,null)) "write_bks",
max(decode(metric_name,'Physical Writes Direct Per Sec',  			average,null)) "write_bks_direct",
max(decode(metric_name,'Redo Generated Per Sec',                    round((average)/1024/1024,1),null)) "redo_mb_s",
max(decode(metric_name,'DB Block Gets Per Sec',                     average,null)) "db_block_gets_s",
max(decode(metric_name,'DB Block Changes Per Sec',                   average,null)) "db_block_changes_s",
max(decode(metric_name,'GC CR Block Received Per Second',            average,null)) "gc_cr_rec_s",
max(decode(metric_name,'GC Current Block Received Per Second',       average,null)) "gc_cu_rec_s",
max(decode(metric_name,'Global Cache Average CR Get Time',           average,null)) "gc_cr_get_cs",
max(decode(metric_name,'Global Cache Average Current Get Time',      average,null)) "gc_cu_get_cs",
max(decode(metric_name,'Global Cache Blocks Corrupted',              average,null)) "gc_bk_corrupted",
max(decode(metric_name,'Global Cache Blocks Lost',                   average,null)) "gc_bk_lost",
max(decode(metric_name,'Active Parallel Sessions',                   average,null)) "px_sess",
max(decode(metric_name,'Active Serial Sessions',                     average,null)) "se_sess",
max(decode(metric_name,'Average Synchronous Single-Block Read Latency', average,null)) "s_blk_r_lat",
max(decode(metric_name,'Cell Physical IO Interconnect Bytes',         round((average)/1024/1024,1),null)) "cell_io_int_mb",
max(decode(metric_name,'Cell Physical IO Interconnect Bytes',         round((maxval)/1024/1024,1),null)) "cell_io_int_mb_max"
  from(
  select  snap_id,num_interval,to_char(end_time,'YY/MM/DD HH24:MI') end_time,instance_number inst,metric_name,round(average,1) average,
  round(maxval,1) maxval
 from dba_hist_sysmetric_summary
where dbid = &DBID
 and snap_id between &SNAP_ID_MIN and &SNAP_ID_MAX
 --and snap_id = 920
 --and instance_number = 4
 and metric_name in ('Host CPU Utilization (%)','CPU Usage Per Sec','Average Active Sessions','Executions Per Sec','Hard Parse Count Per Sec','Logical Reads Per Sec','Logons Per Sec',
 'Physical Read Total Bytes Per Sec','Physical Read Total IO Requests Per Sec','Physical Reads Per Sec','Physical Write Total Bytes Per Sec',
 'Redo Generated Per Sec','User Commits Per Sec','Current Logons Count','DB Block Gets Per Sec','DB Block Changes Per Sec',
 'Database Wait Time Ratio','Database CPU Time Ratio','SQL Service Response Time','Background Time Per Sec',
 'Physical Write Total IO Requests Per Sec','Physical Writes Per Sec','Physical Writes Direct Per Sec','Physical Writes Direct Lobs Per Sec',
 'Physical Reads Direct Per Sec','Physical Reads Direct Lobs Per Sec',
 'GC CR Block Received Per Second','GC Current Block Received Per Second','Global Cache Average CR Get Time','Global Cache Average Current Get Time',
 'Global Cache Blocks Corrupted','Global Cache Blocks Lost',
 'Active Parallel Sessions','Active Serial Sessions','Average Synchronous Single-Block Read Latency','Cell Physical IO Interconnect Bytes'
    )
 )
 group by snap_id,num_interval, end_time,inst
 order by snap_id, end_time,inst;
 
 

prompt 
prompt 
-- ##############################################################################################

column display_value format a50
set wrap off
REPHEADER PAGE LEFT '~~BEGIN-DATABASE-PARAMETERS~~'
REPFOOTER PAGE LEFT '~~END-DATABASE-PARAMETERS~~'
SELECT PARAMETER_NAME,VALUE
FROM DBA_HIST_PARAMETER
WHERE dbid = &DBID
AND snap_id = (SELECT MAX(snap_id) FROM dba_hist_osstat WHERE dbid = &DBID AND instance_number = 1)
   AND instance_number = 1
ORDER BY 1;


prompt 
prompt 
-- ##############################################################################################



REPHEADER PAGE LEFT '~~BEGIN-AVERAGE-ACTIVE-SESSIONS~~'
REPFOOTER PAGE LEFT '~~END-AVERAGE-ACTIVE-SESSIONS~~'
column wait_class format a20

 SELECT snap_id,
    wait_class,
    ROUND (SUM (pSec), 2) avg_sess
   FROM
    (SELECT snap_id,
        wait_class,
        p_tmfg / 1000000 / ela pSec
       FROM
        (SELECT (CAST (s.end_interval_time AS DATE) - CAST (s.begin_interval_time AS DATE)) * 24 *
            3600 ela,
            s.snap_id,
            wait_class,
            e.event_name,
            CASE WHEN s.begin_interval_time = s.startup_time
			-- compare to e.time_waited_micro_fg for 10.2?
                THEN e.&T_WAITED_MICRO_COL
                ELSE e.&T_WAITED_MICRO_COL - lag (e.&T_WAITED_MICRO_COL) over (partition BY
                    event_id, e.dbid, e.instance_number, s.startup_time order by e.snap_id)
            END p_tmfg
           FROM dba_hist_snapshot s,
            dba_hist_system_event e
          WHERE s.dbid = e.dbid
            AND s.dbid = &DBID
            AND s.instance_number = e.instance_number
            AND s.snap_id = e.snap_id
            AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
            AND e.wait_class != 'Idle'
      UNION ALL
         SELECT (CAST (s.end_interval_time AS DATE) - CAST (s.begin_interval_time AS DATE)) * 24 *
            3600 ela,
            s.snap_id,
            t.stat_name wait_class,
            t.stat_name event_name,
            CASE WHEN s.begin_interval_time = s.startup_time
                THEN t.value
                ELSE t.value - lag (value) over (partition BY stat_id, t.dbid, t.instance_number,
                    s.startup_time order by t.snap_id)
            END p_tmfg
           FROM dba_hist_snapshot s,
            dba_hist_sys_time_model t
          WHERE s.dbid = t.dbid
            AND s.dbid = &DBID
            AND s.instance_number = t.instance_number
            AND s.snap_id = t.snap_id
            AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
            AND t.stat_name = 'DB CPU'
        )
		where p_tmfg is not null
    )
GROUP BY snap_id,
    wait_class
ORDER BY snap_id,
    wait_class; 

	
	
prompt 
prompt 
-- ##############################################################################################


REPHEADER OFF
REPFOOTER OFF

define HISTOGRAM_QUERY = ' ' 
column :HISTOGRAM_QUERY_1 new_value HISTOGRAM_QUERY noprint
variable HISTOGRAM_QUERY_1 varchar2(4000)


begin
	if :DB_VERSION_1  >= 11.1 then
		:HISTOGRAM_QUERY_1 := q'!   snap_id,wait_class,event_name,wait_time_milli,sum(wait_count) wait_count
						from(
						SELECT       s.snap_id,
									wait_class,
									h.event_name,
									wait_time_milli,
									CASE WHEN s.begin_interval_time = s.startup_time
										THEN h.wait_count
										ELSE h.wait_count - lag (h.wait_count) over (partition BY
											event_id,wait_time_milli, h.dbid, h.instance_number, s.startup_time order by h.snap_id)
									END wait_count
								   FROM dba_hist_snapshot s,
									DBA_HIST_event_histogram h
								  WHERE s.dbid = h.dbid
									AND s.dbid = &DBID
									AND s.instance_number = h.instance_number
									AND s.snap_id = h.snap_id
									AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
									and event_name in ('cell single block physical read','cell list of blocks physical read','cell multiblock physical read',
													   'db file sequential read','db file scattered read',
													   'log file parallel write','log file sync','free buffer wait')
										  )
							  where wait_count > 0
						group by snap_id,wait_class,event_name,wait_time_milli
							  order by snap_id,event_name,wait_time_milli !';
	else
		:HISTOGRAM_QUERY_1 := q'!  'table not in this version' from dual !';
	end if;

end;
/

select :HISTOGRAM_QUERY_1 from dual;

REPHEADER PAGE LEFT '~~BEGIN-IO-WAIT-HISTOGRAM~~'
REPFOOTER PAGE LEFT '~~END-IO-WAIT-HISTOGRAM~~'
COLUMN EVENT_NAME FORMAT A37
select &HISTOGRAM_QUERY ;
	  
	  
	  
prompt 
prompt 
-- ##############################################################################################


REPHEADER PAGE LEFT '~~BEGIN-IO-OBJECT-TYPE~~'
REPFOOTER PAGE LEFT '~~END-IO-OBJECT-TYPE~~'
COLUMN OBJECT_TYPE FORMAT A15


SELECT s.snap_id,regexp_replace(o.OBJECT_TYPE,'^(TABLE|INDEX).*','\1') OBJECT_TYPE,
       ROUND((sum(s.LOGICAL_READS_DELTA)* &DB_BLOCK_SIZE)/1024/1024/1024,1) logical_read_gb,
	   ROUND((sum(s.PHYSICAL_READS_DELTA)* &DB_BLOCK_SIZE)/1024/1024/1024,1) physical_read_gb,
	   ROUND((sum(s.PHYSICAL_WRITES_DELTA)* &DB_BLOCK_SIZE)/1024/1024/1024,1) physical_write_gb,
	   ROUND((sum(s.SPACE_ALLOCATED_DELTA)/1024/1024/1024),1) GB_ADDED
FROM
  DBA_HIST_SEG_STAT_OBJ o,
  DBA_HIST_SEG_STAT s
where o.dbid = s.dbid
  and o.ts# = s.ts#
  and o.obj# = s.obj#
  and o.dataobj# = s.dataobj#
  and o.dbid = &DBID
				  AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
          AND OBJECT_TYPE != 'UNDEFINED'
  group by s.snap_id,regexp_replace(o.OBJECT_TYPE,'^(TABLE|INDEX).*','\1')
  order by snap_id,object_type;
  
  
  
prompt 
prompt 
-- ##############################################################################################



REPHEADER PAGE LEFT '~~BEGIN-TOP-SQL-SUMMARY~~'
REPFOOTER PAGE LEFT '~~END-TOP-SQL-SUMMARY~~'	

SELECT * FROM(
SELECT s.module,s.action,s.sql_id,avg(s.optimizer_cost) optimizer_cost,
decode(t.command_type,11,'ALTERINDEX',15,'ALTERTABLE',170,'CALLMETHOD',9,'CREATEINDEX',1,'CREATETABLE',
7,'DELETE',50,'EXPLAIN',2,'INSERT',26,'LOCKTABLE',47,'PL/SQLEXECUTE',
3,'SELECT',6,'UPDATE',189,'UPSERT') command_name,
PARSING_SCHEMA_NAME,
DENSE_RANK() OVER
      (ORDER BY sum(EXECUTIONS_DELTA) DESC ) exec_rank,
DENSE_RANK() OVER
      (ORDER BY sum(ELAPSED_TIME_DELTA) DESC ) elap_rank,
DENSE_RANK() OVER
      (ORDER BY sum(BUFFER_GETS_DELTA) DESC ) log_reads_rank,
DENSE_RANK() OVER
      (ORDER BY sum(disk_reads_delta) DESC ) phys_reads_rank,
	  sum(EXECUTIONS_DELTA) execs,
sum(ELAPSED_TIME_DELTA) elap,
sum(BUFFER_GETS_DELTA) log_reads,
round(sum(disk_reads_delta * &DB_BLOCK_SIZE)/1024/1024/1024) phy_read_gb,
      count(distinct plan_hash_value) plan_count,
	  sum(px_servers_execs_delta) px_servers_execs
 FROM dba_hist_sqlstat s,dba_hist_sqltext t
 WHERE s.dbid = &DBID  
  AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
   AND s.dbid = t.dbid
  AND s.sql_id = t.sql_id
  AND PARSING_SCHEMA_NAME NOT IN ('SYS','DBSNMP','SYSMAN')
  GROUP BY s.module,s.action,s.sql_id,t.command_type,PARSING_SCHEMA_NAME)
WHERE elap_rank <= &SQL_TOP_N
 OR phys_reads_rank <= &SQL_TOP_N
 or log_reads_rank <= &SQL_TOP_N
 or exec_rank <= &SQL_TOP_N
 order by elap_rank asc nulls last;

 
REPHEADER PAGE LEFT '~~BEGIN-TOP-SQL-BY-SNAPID~~'
REPFOOTER PAGE LEFT '~~END-TOP-SQL-BY-SNAPID~~'	
with top_n_sql_ids as (select sql_id from(
select sql_id,
DENSE_RANK() OVER
      (ORDER BY sum(ELAPSED_TIME_DELTA) DESC ) elap_rank,
      DENSE_RANK() OVER
      (ORDER BY sum(EXECUTIONS_DELTA) DESC ) exec_rank
  from dba_hist_sqlstat
 WHERE dbid = &DBID  
  AND snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
  AND PARSING_SCHEMA_NAME NOT IN ('SYS','DBSNMP','SYSMAN') 
  GROUP BY sql_id)
WHERE elap_rank <= &SQL_TOP_N * 1.5 
or exec_rank <= &SQL_TOP_N * 1.5 ) 
select * from(
SELECT s.snap_id,PARSING_SCHEMA_NAME,s.module,s.action,s.sql_id,avg(s.optimizer_cost) optimizer_cost,
decode(t.command_type,11,'ALTERINDEX',15,'ALTERTABLE',170,'CALLMETHOD',9,'CREATEINDEX',1,'CREATETABLE',
7,'DELETE',50,'EXPLAIN',2,'INSERT',26,'LOCKTABLE',47,'PL/SQLEXECUTE',
3,'SELECT',6,'UPDATE',189,'UPSERT') command_name,
max(PLAN_HASH_VALUE) plan_hash,sum(EXECUTIONS_DELTA) execs,sum(BUFFER_GETS_DELTA) buffer_gets,sum(ROWS_PROCESSED_DELTA) rows_proc,
round(sum(CPU_TIME_DELTA)/1000000,1) cpu_t_s,round(sum(ELAPSED_TIME_DELTA)/1000000,1) elap_s,
round(sum(disk_reads_delta * &DB_BLOCK_SIZE)/1024/1024,1) read_mb,round(sum(IOWAIT_DELTA)/1000000,1) io_wait,
DENSE_RANK() OVER (PARTITION BY s.snap_id ORDER BY sum(ELAPSED_TIME_DELTA) DESC ) elap_rank,
DENSE_RANK() OVER       (PARTITION BY s.snap_id ORDER BY sum(EXECUTIONS_DELTA) DESC ) exec_rank,
DENSE_RANK() OVER       (PARTITION BY s.snap_id ORDER BY sum(BUFFER_GETS_DELTA) DESC ) log_reads_rank,
DENSE_RANK() OVER       (PARTITION BY s.snap_id ORDER BY sum(disk_reads_delta) DESC ) phys_reads_rank,
      CASE WHEN MAX(PLAN_HASH_VALUE) = LAG(MAX(PLAN_HASH_VALUE), 1, 0) OVER (PARTITION BY s.sql_id ORDER BY snap_id ASC) 
      OR LAG(MAX(PLAN_HASH_VALUE), 1, 0) OVER (PARTITION BY s.sql_id ORDER BY snap_id ASC) = 0 THEN 0
      when count(distinct PLAN_HASH_VALUE) > 1 then 1 else 1 end plan_change,count(distinct PLAN_HASH_VALUE) plans,
      round(sum(disk_reads_delta * &DB_BLOCK_SIZE)/1024/1024/1024) phy_read_gb
  FROM dba_hist_sqlstat s,dba_hist_sqltext t,top_n_sql_ids topn
  WHERE s.dbid = &DBID 
  AND s.dbid = t.dbid
  AND s.sql_id = t.sql_id
  and s.sql_id = topn.sql_id
  AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
  AND PARSING_SCHEMA_NAME NOT IN ('SYS','DBSNMP','SYSMAN') 
  GROUP BY s.snap_id, t.command_type,PARSING_SCHEMA_NAME,s.module,s.action, s.sql_id)
  WHERE elap_rank <= &SQL_TOP_N
  order by snap_id,elap_rank asc nulls last;


  
  
REPHEADER OFF
REPFOOTER OFF
 
spool off