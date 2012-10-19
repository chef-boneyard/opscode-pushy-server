--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: job_nodes; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE job_nodes (
    job_id character(32),
    org_id character(32) NOT NULL,
    node_name text NOT NULL,
    status integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: jobs; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE jobs (
    id character(32) NOT NULL,
    org_id character(32) NOT NULL,
    command text NOT NULL,
    status integer NOT NULL,
    run_timeout integer,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    quorum integer
);


--
-- Name: node_status; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE node_status (
    node_name text NOT NULL,
    org_id character(32) NOT NULL,
    status integer NOT NULL,
    last_updated_by character(32) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: schema_info; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE schema_info (
    version integer DEFAULT 0 NOT NULL
);


--
-- Name: job_nodes_job_id_org_id_node_name_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY job_nodes
    ADD CONSTRAINT job_nodes_job_id_org_id_node_name_key UNIQUE (job_id, org_id, node_name);


--
-- Name: jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY jobs
    ADD CONSTRAINT jobs_pkey PRIMARY KEY (id);


--
-- Name: node_status_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY node_status
    ADD CONSTRAINT node_status_pkey PRIMARY KEY (org_id, node_name);


--
-- Name: job_nodes_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY job_nodes
    ADD CONSTRAINT job_nodes_job_id_fkey FOREIGN KEY (job_id) REFERENCES jobs(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

