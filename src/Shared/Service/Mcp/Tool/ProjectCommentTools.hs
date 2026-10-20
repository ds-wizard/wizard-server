module Shared.Service.Mcp.Tool.ProjectCommentTools where

import Control.Monad.Reader (liftIO)
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Proxy
import qualified Data.UUID as U

import Shared.Api.Resource.Common.PageJM
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Service.Mcp.Tool.McpThirdPartyContent
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.Mcp.Tool.ProjectTools (projectUuidArgument)
import Shared.Service.Project.Comment.ProjectCommentService
import Shared.Service.Project.Event.ProjectEventService
import Shared.Service.User.UserService
import Shared.Service.User.WizardUserMapper
import Shared.Util.JsonSchema
import Shared.Util.Uuid

projectCommentTools :: WizardRequestContextC s m => [McpTool m]
projectCommentTools =
  [ listProjectCommentThreadsTool
  , createProjectCommentThreadTool
  , addProjectCommentTool
  , editProjectCommentTool
  , resolveProjectCommentThreadTool
  , assignProjectCommentThreadTool
  , deleteProjectCommentThreadTool
  , deleteProjectCommentTool
  ]

listProjectCommentThreadsTool :: WizardRequestContextC s m => McpTool m
listProjectCommentThreadsTool =
  markThirdPartyFields [["_embedded", "commentThreads", "comments", "text"]] $
    McpTool
      { name = "list_project_comment_threads"
      , title = "List project comment threads"
      , description =
          "List the comment threads of a project with their comments, newest first, optionally only those at a reply path or only (un)resolved ones; "
            ++ "private threads are shown to editors only. Needs COMMENT on the project"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ pageSchema "commentThreads" (jsonSchema (Proxy :: Proxy ProjectCommentThreadList))
      , annotations = readOnlyAnnotations
      , continuable = True
      , permissions = [_MCP_READ_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = NoTransaction
      , run = withArguments arguments $ \(projectUuid, mPath, mResolved, pageable) ->
          toJSON . toPage "commentThreads" pageable . L.sortOn (Down . (.createdAt)) . concat . M.elems
            <$> getProjectCommentsByProjectUuid projectUuid mPath mResolved
      }
  where
    arguments =
      (,,,)
        <$> projectUuidArgument
        <*> optionalArgument "path" "Only the threads at this reply path"
        <*> optionalArgument "resolved" "Only resolved (true) or only open (false) threads"
        <*> pageableArguments

createProjectCommentThreadTool :: WizardRequestContextC s m => McpTool m
createProjectCommentThreadTool =
  threadTool
    "create_project_comment_thread"
    "Create project comment thread"
    "Start a comment thread at a question of a project (its reply path) with a first comment; private threads are seen by editors only and need EDIT"
    writeAnnotations
    arguments
    (\(projectUuid, path, text, mPrivate) -> createThread projectUuid path text (fromMaybe False mPrivate))
  where
    arguments =
      (,,,)
        <$> projectUuidArgument
        <*> requiredArgument "path" "The reply path of the question (get_authoring_guide topic document-context)"
        <*> textArgument
        <*> optionalArgument "private" "Visible to editors only, false by default"

addProjectCommentTool :: WizardRequestContextC s m => McpTool m
addProjectCommentTool =
  threadTool
    "add_project_comment"
    "Add project comment"
    "Reply to a comment thread of a project with a new comment"
    writeAnnotations
    ((,,) <$> projectUuidArgument <*> threadUuidArgument <*> textArgument)
    (\(projectUuid, threadUuid, text) -> changeThread projectUuid threadUuid (addComment text))

editProjectCommentTool :: WizardRequestContextC s m => McpTool m
editProjectCommentTool =
  threadTool
    "edit_project_comment"
    "Edit project comment"
    "Replace the text of your own comment in an open comment thread of a project"
    idempotentWriteAnnotations
    ((,,,) <$> projectUuidArgument <*> threadUuidArgument <*> commentUuidArgument <*> textArgument)
    (\(projectUuid, threadUuid, commentUuid, text) -> changeThread projectUuid threadUuid (editComment commentUuid text))

resolveProjectCommentThreadTool :: WizardRequestContextC s m => McpTool m
resolveProjectCommentThreadTool =
  threadTool
    "resolve_project_comment_thread"
    "Resolve project comment thread"
    "Resolve a comment thread of a project, or reopen it with resolved false; a thread already in that state stays unchanged"
    idempotentWriteAnnotations
    ((,,) <$> projectUuidArgument <*> threadUuidArgument <*> optionalArgument "resolved" "false reopens the thread, true by default")
    (\(projectUuid, threadUuid, mResolved) -> setThreadResolved projectUuid threadUuid (fromMaybe True mResolved))

assignProjectCommentThreadTool :: WizardRequestContextC s m => McpTool m
assignProjectCommentThreadTool =
  threadTool
    "assign_project_comment_thread"
    "Assign project comment thread"
    "Assign a comment thread of a project to a user who can comment on it (edit, for a private thread), who gets an e-mail about it, or leave user_uuid out to unassign it"
    idempotentWriteAnnotations
    ((,,) <$> projectUuidArgument <*> threadUuidArgument <*> optionalArgument "user_uuid" "The user to assign the thread to (search_users)")
    (\(projectUuid, threadUuid, mUserUuid) -> changeThread projectUuid threadUuid (assignThread projectUuid mUserUuid))

deleteProjectCommentThreadTool :: WizardRequestContextC s m => McpTool m
deleteProjectCommentThreadTool =
  deleteTool
    "delete_project_comment_thread"
    "Delete project comment thread"
    "Delete a comment thread you started, with all its comments"
    ((,) <$> projectUuidArgument <*> threadUuidArgument)
    (\(projectUuid, threadUuid) -> addThreadEvent projectUuid threadUuid deleteThread)

deleteProjectCommentTool :: WizardRequestContextC s m => McpTool m
deleteProjectCommentTool =
  deleteTool
    "delete_project_comment"
    "Delete project comment"
    "Delete one of your own comments in an open comment thread of a project"
    ((,,) <$> projectUuidArgument <*> threadUuidArgument <*> commentUuidArgument)
    (\(projectUuid, threadUuid, commentUuid) -> addThreadEvent projectUuid threadUuid (deleteComment commentUuid))

threadTool :: WizardRequestContextC s m => String -> String -> String -> McpToolAnnotations -> Arguments a -> (a -> m ProjectCommentThreadList) -> McpTool m
threadTool toolName toolTitle toolDescription toolAnnotations arguments action =
  markThirdPartyFields [["comments", "text"]] $
    McpTool
      { name = toolName
      , title = toolTitle
      , description = toolDescription ++ "; needs COMMENT on the project. People with the project open see the change live; the result is the thread"
      , inputSchema = argumentsSchema arguments
      , outputSchema = Just $ describedSchema "The thread, shaped as an item of list_project_comment_threads" (typeSchema "object")
      , annotations = toolAnnotations
      , continuable = False
      , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
      , toolset = Just ProjectsMcpToolset
      , transactionState = Transactional
      , run = withArguments arguments (fmap toJSON . action)
      }

deleteTool :: WizardRequestContextC s m => String -> String -> String -> Arguments a -> (a -> m ()) -> McpTool m
deleteTool toolName toolTitle toolDescription arguments action =
  McpTool
    { name = toolName
    , title = toolTitle
    , description = toolDescription ++ "; needs COMMENT on the project. It cannot be undone"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \parsed -> object [] <$ action parsed
    }

createThread :: WizardRequestContextC s m => U.UUID -> String -> String -> Bool -> m ProjectCommentThreadList
createThread projectUuid path text private = do
  threadUuid <- liftIO generateUuid
  commentUuid <- liftIO generateUuid
  addEvent' projectUuid (\uuid -> AddCommentEventChangeDTO' (AddCommentEventChangeDTO uuid path threadUuid commentUuid text private True))
  getProjectCommentThread projectUuid threadUuid

setThreadResolved :: WizardRequestContextC s m => U.UUID -> U.UUID -> Bool -> m ProjectCommentThreadList
setThreadResolved projectUuid threadUuid resolved = do
  thread <- getProjectCommentThread projectUuid threadUuid
  if thread.resolved == resolved
    then return thread
    else changeThread projectUuid threadUuid (resolveThread resolved)

changeThread :: WizardRequestContextC s m => U.UUID -> U.UUID -> (ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)) -> m ProjectCommentThreadList
changeThread projectUuid threadUuid toEvent = do
  addThreadEvent projectUuid threadUuid toEvent
  getProjectCommentThread projectUuid threadUuid

addThreadEvent :: WizardRequestContextC s m => U.UUID -> U.UUID -> (ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)) -> m ()
addThreadEvent projectUuid threadUuid toEvent = do
  thread <- getProjectCommentThread projectUuid threadUuid
  addEvent' projectUuid =<< toEvent thread

addEvent' :: WizardRequestContextC s m => U.UUID -> (U.UUID -> ProjectEventChangeDTO) -> m ()
addEvent' projectUuid toEvent = addCommentEventToProject projectUuid . toEvent =<< liftIO generateUuid

addComment :: WizardRequestContextC s m => String -> ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
addComment text thread = do
  commentUuid <- liftIO generateUuid
  return $ \uuid -> AddCommentEventChangeDTO' (AddCommentEventChangeDTO uuid thread.path thread.uuid commentUuid text thread.private False)

editComment :: WizardRequestContextC s m => U.UUID -> String -> ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
editComment commentUuid text thread = do
  checkCommentChange thread commentUuid
  return $ \uuid -> EditCommentEventChangeDTO' (EditCommentEventChangeDTO uuid thread.path thread.uuid commentUuid text thread.private)

deleteComment :: WizardRequestContextC s m => U.UUID -> ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
deleteComment commentUuid thread = do
  checkCommentChange thread commentUuid
  return $ \uuid -> DeleteCommentEventChangeDTO' (DeleteCommentEventChangeDTO uuid thread.path thread.uuid commentUuid thread.private)

resolveThread :: Monad m => Bool -> ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
resolveThread True thread = return $ \uuid -> ResolveCommentThreadEventChangeDTO' (ResolveCommentThreadEventChangeDTO uuid thread.uuid thread.path thread.private (length thread.comments))
resolveThread False thread = return $ \uuid -> ReopenCommentThreadEventChangeDTO' (ReopenCommentThreadEventChangeDTO uuid thread.uuid thread.path thread.private (length thread.comments))

assignThread :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
assignThread projectUuid mUserUuid thread = do
  traverse_ (checkCommentThreadAssignee projectUuid thread) mUserUuid
  mUser <- traverse getUserById mUserUuid
  return $ \uuid -> AssignCommentThreadEventChangeDTO' (AssignCommentThreadEventChangeDTO uuid thread.uuid thread.path thread.private (fmap toSuggestion' mUser))

deleteThread :: WizardRequestContextC s m => ProjectCommentThreadList -> m (U.UUID -> ProjectEventChangeDTO)
deleteThread thread = do
  checkCommentThreadDeletion thread
  return $ \uuid -> DeleteCommentThreadEventChangeDTO' (DeleteCommentThreadEventChangeDTO uuid thread.path thread.uuid thread.private)

threadUuidArgument :: Arguments U.UUID
threadUuidArgument = requiredArgument "thread_uuid" "The comment thread"

commentUuidArgument :: Arguments U.UUID
commentUuidArgument = requiredArgument "comment_uuid" "The comment"

textArgument :: Arguments String
textArgument = requiredArgument "text" "The text of the comment (Markdown)"
