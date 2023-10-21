CREATE FUNCTION user_service_update_modified_at()
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

CREATE TABLE users (
    user_id          UUID      DEFAULT GEN_RANDOM_UUID() PRIMARY KEY,
    email_address    TEXT      NOT NULL,
    is_verified      BOOLEAN   DEFAULT FALSE NOT NULL,
    is_deleted       BOOLEAN   DEFAULT FALSE NOT NULL,
    created_at       TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at      TIMESTAMP WITH TIME ZONE
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON users
FOR EACH ROW EXECUTE FUNCTION user_service_update_modified_at();

CREATE TABLE credentials (
    user_id         UUID           PRIMARY KEY REFERENCES users(user_id),
    password_hash   TEXT,
    is_deleted      BOOLEAN        DEFAULT FALSE NOT NULL,
    created_at      TIMESTAMP      WITH TIME ZONE DEFAULT NOW() NOT NULL,
    modified_at     TIMESTAMP      WITH TIME ZONE
);

CREATE TRIGGER update_trigger BEFORE UPDATE ON credentials
FOR EACH ROW EXECUTE FUNCTION user_service_update_modified_at();

