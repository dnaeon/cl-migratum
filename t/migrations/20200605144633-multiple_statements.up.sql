CREATE TABLE country (
    id INTEGER PRIMARY KEY,
    iso_alpha_2 CHARACTER VARYING(2) NOT NULL,
    name CHARACTER VARYING(255) NOT NULL,

    CONSTRAINT country_iso_alpha_2_key UNIQUE (iso_alpha_2)
);
--;;
CREATE INDEX country_name_idx ON country (name);
