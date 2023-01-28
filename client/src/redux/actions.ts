import { AnyAction } from 'redux'
import { ThunkAction } from 'redux-thunk'
import { DELETE_ISSUE, DELETE_PROJECT, LOAD_ISSUES, LOAD_PROJECTS, UPDATE_ISSUE, UPDATE_PROJECT } from './action-types'
import type { RootState } from './'

type AppThunk<ReturnType> = ThunkAction<ReturnType, RootState, any, AnyAction>

export const loadProjects: AppThunk<any>
= async (dispatch, getState, api) => {
  const projects = await api.getApiV1Projects();
  dispatch({ type: LOAD_PROJECTS, payload: projects });
}

export const loadProject: (projectId: ProjectId) => AppThunk<any>
= projectId => async (dispatch, getState, api) => {
  const project = await api.getApiV1ProjectsByProjectId(projectId);
  dispatch({ type: UPDATE_PROJECT, payload: project });
}

export const updateProject: (projectId: ProjectId, updates: any) => AppThunk<any>
= (projectId: ProjectId, updates: any) => async (dispatch, getState, api) => {
  const project = await api.patchApiV1ProjectsByProjectId(projectId, updates);
  dispatch({ type: UPDATE_PROJECT, payload: project });
}

export const createProject: (projectName: string) => AppThunk<any>
= (projectName: string) => async (dispatch, getState, api) => {
  const { projectId } = await api.postApiV1ProjectsCreate();
  const project = await api.patchApiV1ProjectsByProjectId(projectId, [
    { tag: 'ProjectTitle', contents: projectName }
  ]);
  dispatch({ type: UPDATE_PROJECT, payload: project });
  return projectId;
}

export const deleteProject: (projectId: ProjectId) => AppThunk<any>
= (projectId: ProjectId) => async (dispatch, getState, api) => {
  api.deleteApiV1ProjectsDeleteByProjectId(projectId);
  dispatch({ type: DELETE_PROJECT, payload: projectId });
}

export const loadIssues: (projectId: ProjectId) => AppThunk<any>
= (projectId: ProjectId) => async (dispatch, getState, api) => {
  const issues = await api.getApiV1Issues(projectId, null);
  dispatch({ type: LOAD_ISSUES, payload: { projectId, issues } });
}

export const loadIssue: (issueId: IssueId) => AppThunk<any>
= (issueId: IssueId) => async (dispatch, getState, api) => {
  const issue = await api.getApiV1IssuesByIssueId(issueId);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
}

export const updateIssue: (issueId: IssueId, updates: any) => AppThunk<any>
= (issueId: IssueId, updates: any) => async (dispatch, getState, api) => {
  const issue = api.patchApiV1IssuesByIssueId(issueId, updates);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
}

export const createIssue: (projectId: ProjectId, name: string) => AppThunk<any>
= (projectId: ProjectId, name: string) => async (dispatch, getState, api) => {
  const { issueId } = await api.postApiV1IssuesCreate(JSON.stringify(projectId));
  const issue = await api.patchApiV1IssuesByIssueId(issueId, [{
    tag: 'Title', contents: name
  }]);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
  return issueId;
}

export const deleteIssue: (projectId: ProjectId, issueId: IssueId) => AppThunk<any>
= (projectId: ProjectId, issueId: IssueId) => async (dispatch, getState, api) => {
  api.deleteApiV1IssuesDeleteByIssueId(issueId);
  dispatch({ type: DELETE_ISSUE, payload: { projectId, issueId } });
}

