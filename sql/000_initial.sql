CREATE TABLE IF NOT EXISTS rpos_user(
    email VARCHAR(255) PRIMARY KEY NOT NULL,
    password VARCHAR(255),
    reset_key VARCHAR(255),
    created TIMESTAMP NOT NULL DEFAULT NOW(),
    last_login TIMESTAMP
);

CREATE TABLE IF NOT EXISTS rpos_session(
    session_key VARCHAR(255) PRIMARY KEY NOT NULL,
    email VARCHAR(255) NOT NULL,
    created TIMESTAMP NOT NULL DEFAULT NOW(),
    expires TIMESTAMP NOT NULL DEFAULT NOW() + '3 months',

    FOREIGN KEY (email) REFERENCES rpos_user(email)
);
