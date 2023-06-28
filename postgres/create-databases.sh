#!/usr/bin/env bash

set -e

# Creates a PostgreSQL database and user.
#
# User which will be created follows this convention: ${db_name}_user
#
# $1: Database to create
function _create_user_and_db() {
    local _db_name="${1}"
    local _db_user="${_db_name}_user"
    local _db_pass="${_db_name}_p4ss"

    echo "Creating database ${_db_name} (owner ${_db_user}) ..."
    psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
        CREATE USER ${_db_user} PASSWORD '${_db_pass}';
        CREATE DATABASE ${_db_name};
        GRANT ALL PRIVILEGES ON DATABASE ${_db_name} TO ${_db_user};
EOSQL
}

# Main entrypoint
function _main() {
    if [ -z "${POSTGRES_DATABASES}" ]; then
        exit 0
    fi

    for _db_name in $( echo "${POSTGRES_DATABASES}" | sed -e 's|,| |g' ); do
        _create_user_and_db "${_db_name}"
    done
}

_main $*
