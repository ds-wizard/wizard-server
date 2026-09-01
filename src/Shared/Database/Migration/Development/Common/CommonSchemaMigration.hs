module Shared.Database.Migration.Development.Common.CommonSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTypes :: WizardRequestContextC s m => m Int64
dropTypes = do
  logInfo _CMP_MIGRATION "(Type/Common) drop types"
  let sql =
        "DROP TYPE IF EXISTS sem_ver_2_tuple;"
  let action conn = execute_ conn sql
  runDB action

createTypes :: WizardRequestContextC s m => m Int64
createTypes = do
  logInfo _CMP_MIGRATION "(Type/Common) create types"
  createSemVer2TupleType

createSemVer2TupleType :: WizardRequestContextC s m => m Int64
createSemVer2TupleType = do
  let sql =
        "CREATE TYPE sem_ver_2_tuple AS ( \
        \    major INT, \
        \    minor INT \
        \);"
  let action conn = execute_ conn sql
  runDB action

dropFunctions :: WizardRequestContextC s m => m Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Common) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS gravatar_hash; \
        \DROP FUNCTION IF EXISTS create_persistent_command_from_entity_uuid CASCADE; \
        \DROP FUNCTION IF EXISTS create_persistent_command; \
        \DROP FUNCTION IF EXISTS is_outdated; \
        \DROP FUNCTION IF EXISTS major_version; \
        \DROP FUNCTION IF EXISTS minor_version;\
        \DROP FUNCTION IF EXISTS patch_version;\
        \DROP FUNCTION IF EXISTS compare_version;"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: WizardRequestContextC s m => m Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Common) create functions"
  createMajorVersionFn
  createMinorVersionFn
  createPatchVersionFn
  createCompareVersionFn
  createIsOutdatedVersionFn
  createPersistentCommandFunction
  createPersistentCommandFromEntityUuidFunction
  createGravatarFunction

createMajorVersionFn :: WizardRequestContextC s m => m Int64
createMajorVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION major_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    major_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[1] \
        \    INTO major_version; \
        \    RETURN major_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createMinorVersionFn :: WizardRequestContextC s m => m Int64
createMinorVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION minor_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    minor_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[2] \
        \    INTO minor_version; \
        \    RETURN minor_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createPatchVersionFn :: WizardRequestContextC s m => m Int64
createPatchVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION patch_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    patch_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[3] \
        \    INTO patch_version; \
        \    RETURN patch_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createCompareVersionFn :: WizardRequestContextC s m => m Int64
createCompareVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION compare_version(version_1 varchar, version_2 varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    version_order varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN major_version(version_1) = major_version(version_2) \
        \                   THEN CASE \
        \                            WHEN minor_version(version_1) = minor_version(version_2) \
        \                                THEN CASE \
        \                                         WHEN patch_version(version_1) = patch_version(version_2) THEN 'EQ' \
        \                                         WHEN patch_version(version_1) < patch_version(version_2) THEN 'LT' \
        \                                         WHEN patch_version(version_1) > patch_version(version_2) THEN 'GT' \
        \                                END \
        \                            WHEN minor_version(version_1) < minor_version(version_2) THEN 'LT' \
        \                            WHEN minor_version(version_1) > minor_version(version_2) THEN 'GT' \
        \                   END \
        \               WHEN major_version(version_1) < major_version(version_2) THEN 'LT' \
        \               WHEN major_version(version_1) > major_version(version_2) THEN 'GT' \
        \               END \
        \    INTO version_order; \
        \    RETURN version_order; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createIsOutdatedVersionFn :: WizardRequestContextC s m => m Int64
createIsOutdatedVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION is_outdated(version_1 varchar, version_2 varchar) \
        \    RETURNS bool \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    outdated varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN compare_version(version_1, version_2) = 'GT' THEN true \
        \               ELSE false \
        \               END \
        \    INTO outdated; \
        \    RETURN outdated; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createPersistentCommandFunction :: WizardRequestContextC s m => m Int64
createPersistentCommandFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command(component varchar, function varchar, body jsonb, tenant_uuid uuid) RETURNS int AS \
        \$$ \
        \BEGIN \
        \    INSERT INTO persistent_command (uuid, \
        \                                    state, \
        \                                    component, \
        \                                    function, \
        \                                    body, \
        \                                    last_error_message, \
        \                                    attempts, \
        \                                    max_attempts, \
        \                                    tenant_uuid, \
        \                                    created_by, \
        \                                    created_at, \
        \                                    updated_at, \
        \                                    last_trace_uuid) \
        \    VALUES (gen_random_uuid(), \
        \            'NewPersistentCommandState', \
        \            component, \
        \            function, \
        \            body, \
        \            NULL, \
        \            0, \
        \            10, \
        \            tenant_uuid, \
        \            NULL, \
        \            now(), \
        \            now(), \
        \            NULL); \
        \    return 1; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createPersistentCommandFromEntityUuidFunction :: WizardRequestContextC s m => m Int64
createPersistentCommandFromEntityUuidFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_entity_uuid() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \DECLARE \
        \    component varchar; \
        \    function  varchar; \
        \BEGIN \
        \    component := TG_ARGV[0]; \
        \    function := TG_ARGV[1]; \
        \ \
        \    PERFORM create_persistent_command( \
        \            component, \
        \            function, \
        \            jsonb_build_object('uuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createGravatarFunction :: WizardRequestContextC s m => m Int64
createGravatarFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION gravatar_hash(email VARCHAR) RETURNS VARCHAR \
        \    language plpgsql \
        \as \
        \$$ \
        \DECLARE \
        \    hash VARCHAR; \
        \BEGIN \
        \    SELECT md5(lower(trim(email))) \
        \    INTO hash; \
        \    RETURN hash; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
