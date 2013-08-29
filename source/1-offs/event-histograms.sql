set define '&'
set concat '~'
set pagesize 50000

define SNAP_ID_MIN = 13060
--define SNAP_ID_MIN = 1
define SNAP_ID_MAX = 13080

set serveroutput on
set verify off

REPHEADER ON
REPFOOTER ON 

set linesize 1000 
set numwidth 10
set wrap off
set heading on
set trimspool on
set feedback off

REPHEADER PAGE LEFT '~~BEGIN-IO-WAIT-HISTOGRAM~~'
REPFOOTER PAGE LEFT '~~END-IO-WAIT-HISTOGRAM~~'
COLUMN EVENT_NAME FORMAT A37

spool event-histograms.out


select snap_id,wait_class,event_name,wait_time_milli,sum(wait_count) wait_count
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
									AND s.instance_number = h.instance_number
									AND s.snap_id = h.snap_id
									AND s.snap_id BETWEEN &SNAP_ID_MIN and &SNAP_ID_MAX
									/*and event_name in ('cell single block physical read','cell list of blocks physical read','cell multiblock physical read',
													   'db file sequential read','db file scattered read',
													   'log file parallel write','log file sync','free buffer wait')
                                          */
                                    and h.wait_class in ('Cluster','Network','System I/O ','User I/O')
                                    )
							  where wait_count > 0
						group by snap_id,wait_class,event_name,wait_time_milli
							  order by snap_id,event_name,wait_time_milli;


spool off