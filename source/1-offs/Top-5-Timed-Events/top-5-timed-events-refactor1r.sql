SELECT *
FROM
  (SELECT lpad(
    CASE
      WHEN s.instance_number IS NULL
      THEN '*'
      ELSE TO_CHAR(s.instance_number,'999')
    END,4) inststr ,
    wc,
    nm,
    SUM(twt) twt ,
    CASE
      WHEN SUM(twt) = 0
      THEN NULL
      ELSE SUM(tto) /SUM(twt)*100
    END pctto ,
    SUM(ttm)/1000000 ttm ,
    CASE
      WHEN SUM(twt) = 0
      THEN NULL
      ELSE SUM(ttm)/SUM(twt)/1000
    END avtm ,
    CASE
      WHEN SUM(dbt) = 0
      THEN NULL
      ELSE SUM(ttm)/SUM(dbt)*100
    END pctdbt ,
    CASE
      WHEN s.instance_number IS NULL
      THEN AVG(avtm)
    END avavtm ,
    CASE
      WHEN s.instance_number IS NULL
      THEN MIN(avtm)
    END mintm ,
    CASE
      WHEN s.instance_number IS NULL
      THEN MAX(avtm)
    END maxtm ,
    CASE
      WHEN s.instance_number IS NULL
      THEN stddev_samp(avtm)
    END stdtm ,
    CASE
      WHEN s.instance_number IS NULL
      THEN COUNT(*)
    END cnt ,
    dense_rank() over (partition BY s.instance_number order by SUM(ttm) DESC, SUM(twt) DESC) rnk
  FROM (
    (
    /* select events per
    instance */
    SELECT e.event_name nm,
      e.wait_class wc,
      e.instance_number ,
      e.total_waits        - NVL(b.total_waits,0) twt ,
      e.total_timeouts     - NVL(b.total_timeouts,0) tto ,
      (e.time_waited_micro - NVL(b.time_waited_micro,0)) ttm ,
      CASE
        WHEN (e.total_waits - NVL(b.total_waits,0) = 0)
        THEN NULL
        ELSE (e.time_waited_micro - NVL(b.time_waited_micro,0))/ (e.total_waits - NVL(b.total_waits,0))/1000
      END avtm
    FROM dba_hist_system_event e,
      dba_hist_system_event b
    WHERE e.snap_id  = :eid
    AND b.snap_id (+)= :bid
    AND e.dbid       = :dbid
    AND e.dbid       = b.dbid (+)
    --AND e.instance_number MEMBER OF :inst_num
    AND e.instance_number = b.instance_number (+)
    AND e.event_id        = b.event_id (+)
    AND e.event_name      = b.event_name (+)
    AND e.wait_class     <> 'Idle'
    )
  UNION ALL
    (
    /*
    select time for DB CPU */
    SELECT se.stat_name nm,
      NULL wc,
      se.instance_number ,
      NULL twt ,
      NULL tto,
      (se.value - NVL(sb.value,0)) ttm,
      NULL avtm
    FROM dba_hist_sys_time_model se ,
      dba_hist_sys_time_model sb
    WHERE se.snap_id   = :eid
    AND sb.snap_id (+) = :bid
    AND se.dbid        = :dbid
    AND se.dbid        = sb.dbid (+)
    --AND se.instance_number MEMBER OF :inst_num
    AND se.instance_number = sb.instance_number (+)
    AND se.stat_name       = 'DB CPU'
    AND se.stat_name       = sb.stat_name (+)
    AND se.stat_id         = sb.stat_id (+)
    )) s ,
    (SELECT e.instance_number ,
      SUM((e.value - NVL(b.value,0))) dbt
    FROM dba_hist_sys_time_model b,
      dba_hist_sys_time_model e
    WHERE e.dbid = :dbid
    AND e.dbid   = b.dbid (+)
    --AND e.instance_number MEMBER OF :inst_num
    AND e.instance_number = b.instance_number (+)
    AND e.snap_id         = :eid
    AND b.snap_id (+)     = :bid
    AND b.stat_id (+)     = e.stat_id
    AND e.stat_name       = 'DB time'
    GROUP BY e.instance_number
    ) tm
  WHERE s.instance_number = tm.instance_number
  GROUP BY wc,
    nm,
    rollup(s.instance_number)
  )
WHERE rnk <= :top_n_evt
ORDER BY inststr,
  ttm DESC,
  twt DESC,
  nm;
  
-- ##########################################################################################
-- Stage 1

/* events */
SELECT *
FROM
  (SELECT s.snap_id,
    e.instance_number,
    e.event_name nm,
    e.wait_class wc,
     CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.total_waits
      ELSE e.total_waits - lag (e.total_waits ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END twt,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.time_waited_micro
      ELSE e.time_waited_micro - lag (e.time_waited_micro ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END ttm
  FROM dba_hist_snapshot s,
    dba_hist_system_event e
  WHERE s.dbid          = e.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = e.instance_number
  AND s.snap_id         = e.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND e.wait_class     <> 'Idle'
  and e.event_name = 'PX Deq: reap credit'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE ttm IS NOT NULL;



/* select time for DB CPU */
SELECT *
FROM
  (SELECT s.snap_id,
    t.instance_number,
    t.stat_name nm,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN t.value
      ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
    END tm
  FROM dba_hist_snapshot s,
    dba_hist_sys_time_model t
  WHERE s.dbid          = t.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = t.instance_number
  AND s.snap_id         = t.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND t.stat_name = 'DB CPU'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE tm IS NOT NULL;

/* select time for DB Time */
SELECT *
FROM
  (SELECT s.snap_id,
    t.instance_number,
    t.stat_name nm,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN t.value
      ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
    END dbt
  FROM dba_hist_snapshot s,
    dba_hist_sys_time_model t
  WHERE s.dbid          = t.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = t.instance_number
  AND s.snap_id         = t.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND t.stat_name = 'DB time'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE dbt IS NOT NULL;

-- ##########################################################################################
-- Stage 2 - start with the union


(SELECT *
FROM
  (SELECT s.snap_id,
    e.instance_number,
    e.event_name nm,
    e.wait_class wc,
     CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.total_waits
      ELSE e.total_waits - lag (e.total_waits ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END twt,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.time_waited_micro
      ELSE e.time_waited_micro - lag (e.time_waited_micro ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END ttm
  FROM dba_hist_snapshot s,
    dba_hist_system_event e
  WHERE s.dbid          = e.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = e.instance_number
  AND s.snap_id         = e.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND e.wait_class     <> 'Idle'
  and e.event_name = 'PX Deq: reap credit'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE ttm IS NOT NULL
)
union all
(
/* select time for DB CPU */
SELECT *
FROM
  (SELECT s.snap_id,
    t.instance_number,
    t.stat_name nm,
    null wc,
    null twt,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN t.value
      ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
    END ttm
  FROM dba_hist_snapshot s,
    dba_hist_sys_time_model t
  WHERE s.dbid          = t.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = t.instance_number
  AND s.snap_id         = t.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND t.stat_name = 'DB CPU'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE ttm IS NOT NULL);



-- ##########################################################################################
-- Stage 3 - union done, join done, still needs testing

select s.nm,s.wc,1,
CASE
      WHEN SUM(tm.dbt) = 0
      THEN NULL
      ELSE SUM(s.ttm)/SUM(tm.dbt)*100
    END pctdbt 
from(
(SELECT *
FROM
  (SELECT s.snap_id,
    e.instance_number,
    e.event_name nm,
    e.wait_class wc,
     CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.total_waits
      ELSE e.total_waits - lag (e.total_waits ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END twt,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN e.time_waited_micro
      ELSE e.time_waited_micro - lag (e.time_waited_micro ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
    END ttm
  FROM dba_hist_snapshot s,
    dba_hist_system_event e
  WHERE s.dbid          = e.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = e.instance_number
  AND s.snap_id         = e.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND e.wait_class     <> 'Idle'
  --and e.event_name = 'PX Deq: reap credit'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE ttm IS NOT NULL
)
union all
(
/* select time for DB CPU */
SELECT *
FROM
  (SELECT s.snap_id,
    t.instance_number,
    t.stat_name nm,
    null wc,
    null twt,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN t.value
      ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
    END ttm
  FROM dba_hist_snapshot s,
    dba_hist_sys_time_model t
  WHERE s.dbid          = t.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = t.instance_number
  AND s.snap_id         = t.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND t.stat_name = 'DB CPU'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE ttm IS NOT NULL)) s,
(SELECT *
FROM
  (SELECT s.snap_id,
    t.instance_number,
    t.stat_name nm,
    CASE
      WHEN s.begin_interval_time = s.startup_time
      THEN t.value
      ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
    END dbt
  FROM dba_hist_snapshot s,
    dba_hist_sys_time_model t
  WHERE s.dbid          = t.dbid
  AND s.dbid            = :dbid
  AND s.instance_number = t.instance_number
  AND s.snap_id         = t.snap_id
  AND s.snap_id BETWEEN :bid AND :eid
  AND t.stat_name = 'DB time'
  ORDER BY s.snap_id,
    s.instance_number
  )
WHERE dbt IS NOT NULL) tm
WHERE s.instance_number = tm.instance_number
  GROUP BY wc,
    s.nm,
    rollup(s.instance_number);



-- ##########################################################################################
-- Stage 4 - Final

SELECT snap_id,
  wait_class,
  event_name,
  pctdbt
FROM
  (SELECT a.snap_id,
    wait_class,
    event_name,
    ROUND (SUM (pSec), 2) avg_sess,
    SUM(ttm)/1000000 ttm_s,
    SUM(ttm) ttm,
    b.dbt,
    ROUND(SUM(a.ttm)                                             /b.dbt*100,2) pctdbt,
    dense_rank() over (partition BY a.snap_id order by SUM(a.ttm)/b.dbt*100 DESC) rnk
  FROM
    (SELECT snap_id,
      wait_class,
      event_name,
      p_tmfg / 1000000 / ela pSec,
      ttm
    FROM
      (SELECT (CAST (s.end_interval_time AS DATE) - CAST (s.begin_interval_time AS DATE)) * 24 * 3600 ela,
        s.snap_id,
        wait_class,
        e.event_name,
        CASE
          WHEN s.begin_interval_time = s.startup_time
            -- compare to e.time_waited_micro_fg for 10.2?
          THEN e.time_waited_micro
          ELSE e.time_waited_micro - lag (e.time_waited_micro) over (partition BY event_id, e.dbid, e.instance_number, s.startup_time order by e.snap_id)
        END p_tmfg,
        CASE
          WHEN s.begin_interval_time = s.startup_time
          THEN e.time_waited_micro
          ELSE e.time_waited_micro - lag (e.time_waited_micro ) over (partition BY e.instance_number,e.event_name order by e.snap_id)
        END ttm
      FROM dba_hist_snapshot s,
        dba_hist_system_event e
      WHERE s.dbid          = e.dbid
      AND s.dbid            = :dbid
      AND s.instance_number = e.instance_number
      AND s.snap_id         = e.snap_id
      AND s.snap_id BETWEEN :bid AND :eid
      AND e.wait_class != 'Idle'
      UNION ALL
      SELECT (CAST (s.end_interval_time AS DATE) - CAST (s.begin_interval_time AS DATE)) * 24 * 3600 ela,
        s.snap_id,
        t.stat_name wait_class,
        t.stat_name event_name,
        CASE
          WHEN s.begin_interval_time = s.startup_time
          THEN t.value
          ELSE t.value - lag (value) over (partition BY stat_id, t.dbid, t.instance_number, s.startup_time order by t.snap_id)
        END p_tmfg,
        CASE
          WHEN s.begin_interval_time = s.startup_time
          THEN t.value
          ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
        END ttm
      FROM dba_hist_snapshot s,
        dba_hist_sys_time_model t
      WHERE s.dbid          = t.dbid
      AND s.dbid            = :dbid
      AND s.instance_number = t.instance_number
      AND s.snap_id         = t.snap_id
      AND s.snap_id BETWEEN :bid AND :eid
      AND t.stat_name = 'DB CPU'
      )
    WHERE p_tmfg IS NOT NULL
    ) a,
    (SELECT snap_id,
      SUM(dbt) dbt
    FROM
      (SELECT s.snap_id,
        t.instance_number,
        t.stat_name nm,
        CASE
          WHEN s.begin_interval_time = s.startup_time
          THEN t.value
          ELSE t.value - lag (t.value ) over (partition BY s.instance_number order by s.snap_id)
        END dbt
      FROM dba_hist_snapshot s,
        dba_hist_sys_time_model t
      WHERE s.dbid          = t.dbid
      AND s.dbid            = :dbid
      AND s.instance_number = t.instance_number
      AND s.snap_id         = t.snap_id
      AND s.snap_id BETWEEN :bid AND :eid
      AND t.stat_name = 'DB time'
      ORDER BY s.snap_id,
        s.instance_number
      )
    GROUP BY snap_id
    HAVING SUM(dbt) > 0
    ) b
  WHERE a.snap_id = b.snap_id
  GROUP BY a.snap_id,
    a.wait_class,
    a.event_name,
    b.dbt
  HAVING ROUND (SUM (pSec), 2) > 0
  )
WHERE rnk <= 5
ORDER BY snap_id,
  dbt DESC;