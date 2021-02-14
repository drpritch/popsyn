--
-- Scripts to import ILUTE population from CSV files.
--
DROP TABLE ilute.initialpop_i;
CREATE TABLE ilute.initialpop_i(
    personId INTEGER,
    pumiId INTEGER,
    familyId INTEGER,
    dwellingId INTEGER);
CREATE TABLE ilute.initialpop_i_hsk (LIKE ilute.initialpop_i);
CREATE TABLE ilute.initialpop_i_oshawa (LIKE ilute.initialpop_i);
\COPY ilute.initialpop_i FROM '../results/latest/pop_i-Toronto-s000.csv' CSV HEADER
\COPY ilute.initialpop_i_hsk FROM '../results/latest/pop_i-HSK-s000.csv' CSV HEADER
\COPY ilute.initialpop_i_oshawa FROM '../results/latest/pop_i-Oshawa-s000.csv' CSV HEADER
-- Make IDs unique
UPDATE ilute.initialpop_i_hsk SET personId=personId + prev.max
    FROM (SELECT max(personId) FROM ilute.initialpop_i) AS prev
    WHERE personId>=0;
UPDATE ilute.initialpop_i_oshawa SET personId=personId + prev.max
    FROM (SELECT max(personId) FROM ilute.initialpop_i_hsk) AS prev
    WHERE personId>=0;
ALTER TABLE ilute.initialpop_i ADD PRIMARY KEY (personId);
GRANT SELECT ON ilute.initialpop_i TO GROUP transport_read;



DROP TABLE ilute.initialpop_i_collective;
CREATE TABLE ilute.initialpop_i_collective(
    personcId INTEGER,
    pumiId INTEGER,
    ctcode INTEGER,
    tts96 INTEGER);
CREATE TABLE ilute.initialpop_i_collective_hsk
    (LIKE ilute.initialpop_i_collective);
CREATE TABLE ilute.initialpop_i_collective_oshawa
    (LIKE ilute.initialpop_i_collective);

\COPY ilute.initialpop_i_collective FROM '../results/latest/pop_ic-Toronto-s000.csv' CSV HEADER
\COPY ilute.initialpop_i_collective_hsk FROM '../results/latest/pop_ic-HSK-s000.csv' CSV HEADER
\COPY ilute.initialpop_i_collective_oshawa FROM '../results/latest/pop_ic-Oshawa-s000.csv' CSV HEADER
UPDATE ilute.initialpop_i_collective_hsk SET personcId=personcId + prev.max
    FROM (SELECT max(personcId) FROM ilute.initialpop_i_collective) AS prev
    WHERE personcId>=0;
UPDATE ilute.initialpop_i_collective_oshawa SET personcId=personcId + prev.max
    FROM (SELECT max(personcId) FROM ilute.initialpop_i_collective_hsk) AS prev
    WHERE personcId>=0;
ALTER TABLE ilute.initialpop_i_collective ADD PRIMARY KEY (personcId);
GRANT SELECT ON ilute.initialpop_i_collective TO GROUP transport_read;




DROP TABLE ilute.initialpop_f;
CREATE TABLE ilute.initialpop_f(
    familyId INTEGER,
    pumfId INTEGER,
    dwellingId INTEGER);
CREATE TABLE ilute.initialpop_f_hsk (LIKE ilute.initialpop_f);
CREATE TABLE ilute.initialpop_f_oshawa (LIKE ilute.initialpop_f);

\COPY ilute.initialpop_f FROM '../results/latest/pop_f-Toronto-s000.csv' CSV HEADER
\COPY ilute.initialpop_f_hsk FROM '../results/latest/pop_f-HSK-s000.csv' CSV HEADER
\COPY ilute.initialpop_f_oshawa FROM '../results/latest/pop_f-Oshawa-s000.csv' CSV HEADER
UPDATE ilute.initialpop_i_hsk SET familyId=familyId + prev.max
    FROM (SELECT max(familyId) FROM ilute.initialpop_f) AS prev
    WHERE familyId>=0;
UPDATE ilute.initialpop_f_hsk SET familyId=familyId + prev.max
    FROM (SELECT max(familyId) FROM ilute.initialpop_f) AS prev
    WHERE familyId>=0;
UPDATE ilute.initialpop_i_oshawa SET familyId=familyId + prev.max
    FROM (SELECT max(familyId) FROM ilute.initialpop_f_hsk) AS prev
    WHERE familyId>=0;
UPDATE ilute.initialpop_f_oshawa SET familyId=familyId + prev.max
    FROM (SELECT max(familyId) FROM ilute.initialpop_f_hsk) AS prev
    WHERE familyId>=0;
ALTER TABLE ilute.initialpop_f ADD PRIMARY KEY (familyId);
GRANT SELECT ON ilute.initialpop_f TO GROUP transport_read;




DROP TABLE ilute.initialpop_d;
CREATE TABLE ilute.initialpop_d(
    dwellingId INTEGER,
    pumhId INTEGER,
    ctcode INTEGER,
    tts96 INTEGER);
CREATE TABLE ilute.initialpop_d_hsk (LIKE ilute.initialpop_d);
CREATE TABLE ilute.initialpop_d_oshawa (LIKE ilute.initialpop_d);

\COPY ilute.initialpop_d FROM '../results/latest/pop_d-Toronto-s000.csv' CSV HEADER
\COPY ilute.initialpop_d_hsk FROM '../results/latest/pop_d-HSK-s000.csv' CSV HEADER
\COPY ilute.initialpop_d_oshawa FROM '../results/latest/pop_d-Oshawa-s000.csv' CSV HEADER
UPDATE ilute.initialpop_i_hsk SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d) AS prev
    WHERE dwellingId>=0;
UPDATE ilute.initialpop_f_hsk SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d) AS prev
    WHERE dwellingId>=0;
UPDATE ilute.initialpop_d_hsk SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d) AS prev
    WHERE dwellingId>=0;
UPDATE ilute.initialpop_i_oshawa SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d_hsk) AS prev
    WHERE dwellingId>=0;
UPDATE ilute.initialpop_f_oshawa SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d_hsk) AS prev
    WHERE dwellingId>=0;
UPDATE ilute.initialpop_d_oshawa SET dwellingId=dwellingId + prev.max
    FROM (SELECT max(dwellingId) FROM ilute.initialpop_d_hsk) AS prev
    WHERE dwellingId>=0;
ALTER TABLE ilute.initialpop_d ADD PRIMARY KEY (dwellingId);
GRANT SELECT ON ilute.initialpop_d TO GROUP transport_read;


INSERT INTO ilute.initialpop_i SELECT * FROM ilute.initialpop_i_hsk;
INSERT INTO ilute.initialpop_i SELECT * FROM ilute.initialpop_i_oshawa;
INSERT INTO ilute.initialpop_i_collective
    SELECT * FROM ilute.initialpop_i_collective_hsk;
INSERT INTO ilute.initialpop_i_collective
    SELECT * FROM ilute.initialpop_i_collective_oshawa;
INSERT INTO ilute.initialpop_f SELECT * FROM ilute.initialpop_f_hsk;
INSERT INTO ilute.initialpop_f SELECT * FROM ilute.initialpop_f_oshawa;
INSERT INTO ilute.initialpop_d SELECT * FROM ilute.initialpop_d_hsk;
INSERT INTO ilute.initialpop_d SELECT * FROM ilute.initialpop_d_oshawa;
DROP TABLE ilute.initialpop_i_hsk;
DROP TABLE ilute.initialpop_i_oshawa;
DROP TABLE ilute.initialpop_i_collective_hsk;
DROP TABLE ilute.initialpop_i_collective_oshawa;
DROP TABLE ilute.initialpop_f_hsk;
DROP TABLE ilute.initialpop_f_oshawa;
DROP TABLE ilute.initialpop_d_hsk;
DROP TABLE ilute.initialpop_d_oshawa;
