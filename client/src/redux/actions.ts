import * as A from './action-types'
import { store } from './store'

type API = any
type AppAction<T> = (dispatch: (arg: any) => typeof store.dispatch, getState: () => RootState, api: API) => Promise<T>;

export const loadProjects: AppAction<void>
= async (dispatch, _getState, api) => {
  const projects = await api.getProjects();
  dispatch({ type: A.LOAD_PROJECTS, payload: projects });
}

export const loadProject: (projectId: ProjectId) => AppAction<void>
= projectId => async (dispatch, getState, api) => {
  const project = await api.getProject(projectId);
  dispatch({ type: A.UPDATE_PROJECT, payload: project });
}

export const createProject: (arg: { projectName: string, projectDescription: string }) => AppAction<ProjectId>
= arg => async (dispatch, _getState, api) => {
  const project = await api.createProject(arg);
  dispatch({ type: A.UPDATE_PROJECT, payload: project });
  return project.projectId
}

export const updateProject: (projectId: ProjectId, arg: { projectName?: string, projectDescription?: string }) => AppAction<any>
= (projectId, arg) => async (dispatch, _getState, api) => {
  const project = await api.updateProject(projectId, arg);
  dispatch({ type: A.UPDATE_PROJECT, payload: project });
}

export const deleteProject: (projectId: ProjectId) => AppAction<any>
= projectId => async (dispatch, _getState, api) => {
  await api.deleteProject(projectId);
  dispatch({ type: A.DELETE_PROJECT, payload: projectId });
}

export const loadIssues: (projectId: ProjectId) => AppAction<void>
= projectId => async (_dispatch, _getState, _api) => {
    // TODO
}

export const loadIssue: (issueId: IssueId) => AppAction<void>
= issueId => async (_dispatch, _getState, _api) => {
    // TODO
}

export const createIssue: (projectId: ProjectId, args: any) => AppAction<ProjectId>
= projectId => async (_dispatch, _getState, _api) => {
    // TODO
    return "UNIMPLEMENTED"
}

export const updateIssue: (issueId: IssueId, updates: any) => AppAction<void>
= (issueId, update) => async (_dispatch, _getState, _api) => {
    // TODO
}

export const deleteIssue: (issueId: IssueId) => AppAction<void>
= issueId => async (_dipatch, _getState, _api) => {
    // TODO
}

