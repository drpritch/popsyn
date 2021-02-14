--
-- PostgreSQL script to create the rooms visualization map data from the
-- ILUTE population.
--
SELECT ctcode,room,count(1) INTO temp
    FROM ilute.initialpop_d AS pop
        JOIN census.pum_h_1986_canada AS pum ON (pop.pumhid=pum.id)
    GROUP BY ctcode,room;
SELECT ctcode,sum(count) AS count INTO temp2 FROM temp GROUP BY ctcode;
ALTER TABLE temp2 ADD COLUMN pct2 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct3 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct4 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct5 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct6 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct7 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct8 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct9 FLOAT;
ALTER TABLE temp2 ADD COLUMN pct10 FLOAT;
UPDATE temp2 SET pct2=0;
UPDATE temp2 SET pct3=0;
UPDATE temp2 SET pct4=0;
UPDATE temp2 SET pct5=0;
UPDATE temp2 SET pct6=0;
UPDATE temp2 SET pct7=0;
UPDATE temp2 SET pct8=0;
UPDATE temp2 SET pct9=0;
UPDATE temp2 SET pct10=0;
UPDATE temp2 SET pct2=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=2 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct3=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=3 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct4=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=4 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct5=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=5 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct6=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=6 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct7=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=7 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct8=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=8 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct9=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=9 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
UPDATE temp2 SET pct10=100.0 * g.sum / count FROM
    (SELECT ctcode,sum(count) FROM temp WHERE room>=10 GROUP BY ctcode) AS g
    WHERE temp2.ctcode = g.ctcode;
ALTER TABLE temp2 ADD COLUMN mean FLOAT;
ALTER TABLE temp2 ADD COLUMN median FLOAT;
UPDATE temp2 SET median=10;
UPDATE temp2 SET median=2 WHERE pct2 >= 50 AND pct3 < 50;
UPDATE temp2 SET median=3 WHERE pct3 >= 50 AND pct4 < 50;
UPDATE temp2 SET median=4 WHERE pct4 >= 50 AND pct5 < 50;
UPDATE temp2 SET median=5 WHERE pct5 >= 50 AND pct6 < 50;
UPDATE temp2 SET median=6 WHERE pct6 >= 50 AND pct7 < 50;
UPDATE temp2 SET median=7 WHERE pct7 >= 50 AND pct8 < 50;
UPDATE temp2 SET median=8 WHERE pct8 >= 50 AND pct9 < 50;
UPDATE temp2 SET median=9 WHERE pct9 >= 50 AND pct10 < 50;

SELECT * INTO temp3
    FROM temp2
        JOIN census.boundary_ct1986_coz AS b ON (temp2.ctcode=b.ctuid);
ALTER TABLE temp3 ADD COLUMN id SERIAL PRIMARY KEY;
