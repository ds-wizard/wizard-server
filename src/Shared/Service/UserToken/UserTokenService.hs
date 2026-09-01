module Shared.Service.UserToken.UserTokenService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import qualified Data.Cache as C
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import GHC.Records

import Shared.Database.DAO.Common
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Localization.Messages.User.Public
import Shared.Model.Context.RequestContext
import Shared.Model.Error.Error
import Shared.Model.User.UserToken
import Shared.Model.User.UserTokenList
import Shared.Util.Logger

getTokens :: RequestContextC s sc m => UserTokenType -> Maybe U.UUID -> m [UserTokenList]
getTokens tokenType mCurrentTokenUuid = do
  context <- ask
  let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
  case mIdentityUuid of
    Just identityUuid -> findUserTokensByUserUuidAndTypeAndTokenUuid identityUuid tokenType mCurrentTokenUuid
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokensExceptCurrentSession
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => String
  -> m ()
deleteTokensExceptCurrentSession tokenValue =
  runInTransaction logInfoI logWarnI $ do
    context <- ask
    let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
    case mIdentityUuid of
      Just identityUuid -> do
        userTokens <- findUserTokensByUserUuidAndType identityUuid "LoginUserTokenType"
        traverse_ (deleteUserTokenByUuid . ((.uuid))) . filter (\ut -> ut.value /= tokenValue) $ userTokens
      Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokenByUuid
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => U.UUID
  -> m ()
deleteTokenByUuid uuid = do
  _ <- findUserTokenByUuid uuid
  deleteUserTokenByUuid uuid
  return ()

deleteTokensByTenantUuid
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => U.UUID
  -> m ()
deleteTokensByTenantUuid tenantUuid = do
  userTokens <- findUserTokensByTenantUuid tenantUuid
  traverse_ (\t -> deleteUserTokenByUuidAndTenantUuid t.uuid tenantUuid) userTokens

cleanTokens
  :: ( RequestContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     , HasField "cache'" sc scCache
     , HasField "dataEnabled'" scCache Bool
     )
  => m ()
cleanTokens = do
  deletedUserTokens <- deleteUserTokensWithExpiration
  logInfoI _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
