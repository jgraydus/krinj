CREATE FUNCTION entity_service_update_modified_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.created_at = OLD.created_at;  -- prevent changing the created_at timestamp
    IF row(NEW.*) IS DISTINCT FROM row(OLD.*) THEN
        NEW.modified_at = NOW();
        RETURN NEW;
    ELSE
        RETURN OLD;
    END IF;
END;
$$ LANGUAGE 'plpgsql';

CREATE TABLE projects (
    project_id       UUID          DEFAULT GEN_RANDOM_UUID() PRIMARY KEY,
    name             TEXT          NOT NULL,
    description      TEXT          NOT NULL,
    created_at       TIMESTAMP     WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at      TIMESTAMP     WITH TIME ZONE
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON projects
FOR EACH ROW EXECUTE FUNCTION entity_service_update_modified_at();

CREATE TABLE entity_types (
    entity_type_id   UUID          DEFAULT GEN_RANDOM_UUID() PRIMARY KEY,
    project_id       UUID          REFERENCES projects(project_id) NOT NULL,
    name             TEXT          NOT NULL,
    descriptor       JSONB         NOT NULL,
    created_at       TIMESTAMP     WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at      TIMESTAMP     WITH TIME ZONE,
    UNIQUE (project_id, name)
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON entity_types
FOR EACH ROW EXECUTE FUNCTION entity_service_update_modified_at();

CREATE TABLE entities (
    entity_id        UUID          DEFAULT GEN_RANDOM_UUID() PRIMARY KEY,
    project_id       UUID          REFERENCES projects(project_id) NOT NULL,
    entity_type_id   UUID          REFERENCES entity_types(entity_type_id) NOT NULL,
    created_at       TIMESTAMP     WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at      TIMESTAMP     WITH TIME ZONE
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON entities
FOR EACH ROW EXECUTE FUNCTION entity_service_update_modified_at();

CREATE TABLE attributes (
    entity_id        UUID          REFERENCES entities(entity_id) NOT NULL,
    name             TEXT          NOT NULL,
    value            JSONB         NOT NULL,
    created_at       TIMESTAMP     WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at      TIMESTAMP     WITH TIME ZONE,
    PRIMARY KEY (entity_id, name)
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON attributes
FOR EACH ROW EXECUTE FUNCTION entity_service_update_modified_at();

CREATE TABLE relationships (
    relationship_id      UUID          DEFAULT GEN_RANDOM_UUID() PRIMARY KEY,
    subject_id           UUID          REFERENCES entities(entity_id) NOT NULL,
    object_id            UUID          REFERENCES entities(entity_id) NOT NULL,
    relationship_type    TEXT          NOT NULL,
    created_at           TIMESTAMP     WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at          TIMESTAMP     WITH TIME ZONE
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON relationships
FOR EACH ROW EXECUTE FUNCTION entity_service_update_modified_at();

