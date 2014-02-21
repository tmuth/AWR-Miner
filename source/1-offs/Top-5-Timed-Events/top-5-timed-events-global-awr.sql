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