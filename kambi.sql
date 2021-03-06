-- Avtor: dr. Robert Leskovar
-- Generated by Oracle SQL Developer Data Modeler 19.4.0.350.1424
--   at:        2021-02-09 22:07:35 CET
--   site:      Oracle Database 12c
--   type:      Oracle Database 12c



CREATE TABLE ib_delo_kompetenca (
    id_delo_kompetenca  NUMBER NOT NULL,
    naziv               VARCHAR2(50) NOT NULL,
    navodila            VARCHAR2(4000 CHAR),
    logo                VARCHAR2(2000 CHAR)
);

ALTER TABLE ib_delo_kompetenca ADD CONSTRAINT id_delo_kompetenca_pk PRIMARY KEY ( id_delo_kompetenca );

CREATE TABLE ib_kandidat (
    id_kandidat  NUMBER NOT NULL,
    user_ip      VARCHAR2(50),
    user_mac     VARCHAR2(4000),
    start_date   DATE,
    end_date     DATE,
    verify       NUMBER
);

ALTER TABLE ib_kandidat ADD CONSTRAINT ib_kandidat_pk PRIMARY KEY ( id_kandidat );

CREATE TABLE ib_kandidat_poklic (
    id_kandidat_poklic  NUMBER NOT NULL,
    id_kandidat         NUMBER NOT NULL,
    id_poklic           NUMBER NOT NULL,
    kandidat1           NUMBER,
    poklic1             NUMBER,
    kandidat0           NUMBER,
    poklic0             NUMBER
);

ALTER TABLE ib_kandidat_poklic ADD CONSTRAINT ib_kandidat_poklic_pk PRIMARY KEY ( id_kandidat_poklic );

CREATE TABLE ib_odgovor (
    id_odgovor    NUMBER NOT NULL,
    id_kandidat   NUMBER NOT NULL,
    id_vprasanje  NUMBER NOT NULL,
    odgovor       VARCHAR2(50) NOT NULL,
    cas           NUMBER
);

ALTER TABLE ib_odgovor ADD CONSTRAINT ib_odgovor_pk PRIMARY KEY ( id_odgovor );

CREATE TABLE ib_podrocje (
    opis         VARCHAR2(2000),
    id_podrocje  NUMBER NOT NULL
);

ALTER TABLE ib_podrocje ADD CONSTRAINT ib_podrocje_pk PRIMARY KEY ( id_podrocje );

CREATE TABLE ib_poklic (
    id_poklic  NUMBER NOT NULL,
    naziv      VARCHAR2(200 CHAR),
    opis       VARCHAR2(4000)
);

ALTER TABLE ib_poklic ADD CONSTRAINT ib_poklic_pk PRIMARY KEY ( id_poklic );

CREATE TABLE ib_poklic_povezave (
    id_poklic_povezava  NUMBER NOT NULL,
    id_poklic           NUMBER NOT NULL,
    povezava            VARCHAR2(400 CHAR) NOT NULL,
    povezava_naziv      VARCHAR2(400 CHAR)
);

ALTER TABLE ib_poklic_povezave ADD CONSTRAINT ib_poklic_povezave_pk PRIMARY KEY ( id_poklic_povezava );

CREATE TABLE ib_poklic_znacilnosti (
    id_poklic_znacilnosti  NUMBER NOT NULL,
    id_poklic              NUMBER NOT NULL,
    id_podrocje            NUMBER NOT NULL,
    id_delo_kompetenca     NUMBER NOT NULL
);

ALTER TABLE ib_poklic_znacilnosti ADD CONSTRAINT ib_poklic_znacilnosti_pk PRIMARY KEY ( id_poklic_znacilnosti );

CREATE TABLE ib_vprasanje (
    id_vprasanje        NUMBER NOT NULL,
    besedilovpr         VARCHAR2(256) NOT NULL,
    id_podrocje         NUMBER NOT NULL,
    id_predloga         VARCHAR2(30) NOT NULL,
    zaporedje           NUMBER(3) NOT NULL,
    id_delo_kompetenca  NUMBER NOT NULL
);

ALTER TABLE ib_vprasanje ADD CONSTRAINT ib_vprasanje_pk PRIMARY KEY ( id_vprasanje );

CREATE TABLE ib_vrprasanje_predloga (
    id_vprasanje_predloga  VARCHAR2(30) NOT NULL,
    json_values            CLOB,
    opis                   VARCHAR2(512) NOT NULL,
    navodila               VARCHAR2(4000 CHAR)
);

ALTER TABLE ib_vrprasanje_predloga ADD CONSTRAINT ib_vrprasanje_predloga_json CHECK ( json_values IS JSON );

ALTER TABLE ib_vrprasanje_predloga ADD CONSTRAINT ib_vprasanje_predloga_pk PRIMARY KEY ( id_vprasanje_predloga );

ALTER TABLE ib_delo_kompetenca
    ADD CONSTRAINT ib_delo_kompetenca_fk FOREIGN KEY ( id_delo_kompetenca )
        REFERENCES ib_delo_kompetenca ( id_delo_kompetenca );

ALTER TABLE ib_kandidat_poklic
    ADD CONSTRAINT ib_kandidat_poklic_kandidat_fk FOREIGN KEY ( id_kandidat )
        REFERENCES ib_kandidat ( id_kandidat );

ALTER TABLE ib_kandidat_poklic
    ADD CONSTRAINT ib_kandidat_poklic_poklic_fk FOREIGN KEY ( id_poklic )
        REFERENCES ib_poklic ( id_poklic )
            ON DELETE CASCADE;

ALTER TABLE ib_odgovor
    ADD CONSTRAINT ib_odgovor_kandidat_fk FOREIGN KEY ( id_kandidat )
        REFERENCES ib_kandidat ( id_kandidat );

ALTER TABLE ib_odgovor
    ADD CONSTRAINT ib_odgovor_vprasanje_fk FOREIGN KEY ( id_vprasanje )
        REFERENCES ib_vprasanje ( id_vprasanje );

ALTER TABLE ib_poklic_povezave
    ADD CONSTRAINT ib_poklic_povezave_poklic_fk FOREIGN KEY ( id_poklic )
        REFERENCES ib_poklic ( id_poklic );

ALTER TABLE ib_poklic_znacilnosti
    ADD CONSTRAINT ib_poklic_znac_komp_fk FOREIGN KEY ( id_delo_kompetenca )
        REFERENCES ib_delo_kompetenca ( id_delo_kompetenca );

ALTER TABLE ib_poklic_znacilnosti
    ADD CONSTRAINT ib_poklic_znac_podrocje_fk FOREIGN KEY ( id_podrocje )
        REFERENCES ib_podrocje ( id_podrocje );

ALTER TABLE ib_poklic_znacilnosti
    ADD CONSTRAINT ib_poklic_znac_poklic_fk FOREIGN KEY ( id_poklic )
        REFERENCES ib_poklic ( id_poklic )
            ON DELETE CASCADE;

ALTER TABLE ib_vprasanje
    ADD CONSTRAINT ib_vprasanje_podrocje_fk FOREIGN KEY ( id_podrocje )
        REFERENCES ib_podrocje ( id_podrocje )
            ON DELETE CASCADE;

ALTER TABLE ib_vprasanje
    ADD CONSTRAINT ib_vprasanje_predloga_fk FOREIGN KEY ( id_predloga )
        REFERENCES ib_vrprasanje_predloga ( id_vprasanje_predloga )
            ON DELETE CASCADE;

CREATE SEQUENCE ib_seq START WITH 1 NOCACHE ORDER;

CREATE OR REPLACE TRIGGER ib_delo_kompetenca_bi BEFORE
    INSERT ON ib_delo_kompetenca
    FOR EACH ROW
    WHEN ( new.id_delo_kompetenca IS NULL )
BEGIN
    :new.id_delo_kompetenca := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_kandidat_poklic_bi BEFORE
    INSERT ON ib_kandidat_poklic
    FOR EACH ROW
    WHEN ( new.id_kandidat_poklic IS NULL )
BEGIN
    :new.id_kandidat_poklic := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_odgovor_bi BEFORE
    INSERT ON ib_odgovor
    FOR EACH ROW
    WHEN ( new.id_odgovor IS NULL )
BEGIN
    :new.id_odgovor := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_podrocje_bi BEFORE
    INSERT ON ib_podrocje
    FOR EACH ROW
    WHEN ( new.id_podrocje IS NULL )
BEGIN
    :new.id_podrocje := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_poklic_bi BEFORE
    INSERT ON ib_poklic
    FOR EACH ROW
    WHEN ( new.id_poklic IS NULL )
BEGIN
    :new.id_poklic := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_poklic_povezave_bi BEFORE
    INSERT ON ib_poklic_povezave
    FOR EACH ROW
    WHEN ( new.id_poklic_povezava IS NULL )
BEGIN
    :new.id_poklic_povezava := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_poklic_znacilnosti_bi BEFORE
    INSERT ON ib_poklic_znacilnosti
    FOR EACH ROW
    WHEN ( new.id_poklic_znacilnosti IS NULL )
BEGIN
    :new.id_poklic_znacilnosti := ib_seq.nextval;
END;
/

CREATE OR REPLACE TRIGGER ib_vprasanje_bi BEFORE
    INSERT ON ib_vprasanje
    FOR EACH ROW
    WHEN ( new.id_vprasanje IS NULL )
BEGIN
    :new.id_vprasanje := ib_seq.nextval;
END;
/
