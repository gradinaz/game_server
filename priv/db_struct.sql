--
-- PostgreSQL database dump
--

-- Dumped from database version 10.9 (Ubuntu 10.9-1.pgdg18.04+1)
-- Dumped by pg_dump version 11.4 (Ubuntu 11.4-1.pgdg18.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: game_server; Type: SCHEMA; Schema: -; Owner: dba
--

CREATE SCHEMA game_server;


ALTER SCHEMA game_server OWNER TO dba;

--
-- Name: session_id; Type: SEQUENCE; Schema: game_server; Owner: dba
--

CREATE SEQUENCE game_server.session_id
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE game_server.session_id OWNER TO dba;


create SEQUENCE game_server.user_info_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE game_server.user_info_id_seq OWNER TO dba;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: user_info; Type: TABLE; Schema: game_server; Owner: dba
--

CREATE TABLE game_server.user_info (
    id bigint DEFAULT nextval('game_server.user_info_id_seq'::regclass) NOT NULL,
    nickname character varying NOT NULL,
    level integer NOT NULL,
    coins integer NOT NULL,
    stars integer NOT NULL
);


ALTER TABLE game_server.user_info OWNER TO dba;

ALTER SEQUENCE game_server.user_info_id_seq OWNED BY game_server.user_info.id;


--
-- Data for Name: user_info; Type: TABLE DATA; Schema: game_server; Owner: dba
--

--
-- Name: session_id; Type: SEQUENCE SET; Schema: game_server; Owner: dba
--

SELECT pg_catalog.setval('game_server.session_id', 1, false);


--
-- Name: user_info user_info_pkey; Type: CONSTRAINT; Schema: game_server; Owner: dba
--

ALTER TABLE ONLY game_server.user_info
    ADD CONSTRAINT user_info_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

