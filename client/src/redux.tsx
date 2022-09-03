import * as R from 'ramda'
import { applyMiddleware, createStore } from 'redux';
import thunk from 'redux-thunk'
import { createSelector } from 'reselect'
import api from './api'

const LOAD_PROJECTS = 'projects/load';
const UPDATE_PROJECT = 'projects/update';
const DELETE_PROJECT = 'projects/delete';
const LOAD_ISSUES = 'issues/load';
const UPDATE_ISSUE = 'issues/update';
const DELETE_ISSUE = 'issues/delete';

export const loadProjects = async (dispatch, getState, api) => {
  const projects = await api.getApiV1Projects();
  dispatch({ type: LOAD_PROJECTS, payload: projects });
}

export const loadProject = projectId => async (dispatch, getState, api) => {
  const project = await api.getApiV1ProjectsByProjectId(projectId);
  dispatch({ type: UPDATE_PROJECT, payload: project });
}

export const updateProject = (projectId, updates) => async (dispatch, getState, api) => {
  const project = await api.patchApiV1ProjectsByProjectId(projectId, updates);
  dispatch({ type: UPDATE_PROJECT, payload: project });
}

export const createProject = projectName => async (dispatch, getState, api) => {
  const { projectId } = await api.postApiV1ProjectsCreate();
  const project = await api.patchApiV1ProjectsByProjectId(projectId, [
    { tag: 'ProjectTitle', contents: projectName }
  ]);
  dispatch({ type: UPDATE_PROJECT, payload: project });
  return projectId;
}

export const deleteProject = projectId => async (dispatch, getState, api) => {
  api.deleteApiV1ProjectsDeleteByProjectId(projectId);
  dispatch({ type: DELETE_PROJECT, payload: projectId });
}

export const loadIssues = projectId => async (dispatch, getState, api) => {
  const issues = await api.getApiV1Issues(projectId, null);
  dispatch({ type: LOAD_ISSUES, payload: { projectId, issues } });
}

export const loadIssue = issueId => async (dispatch, getState, api) => {
  const issue = await api.getApiV1IssuesByIssueId(issueId);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
}

export const updateIssue = (issueId, updates) => async (dispatch, getState, api) => {
  const issue = api.patchApiV1IssuesByIssueId(issueId, updates);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
}

export const createIssue = (projectId, name) => async (dispatch, getState, api) => {
  const { issueId } = await api.postApiV1IssuesCreate(JSON.stringify(projectId));
  const issue = await api.patchApiV1IssuesByIssueId(issueId, [{
    tag: 'Title', contents: name
  }]);
  dispatch({ type: UPDATE_ISSUE, payload: issue });
  return issueId;
}

export const deleteIssue = (projectId, issueId) => async (dispatch, getState, api) => {
  api.deleteApiV1IssuesDeleteByIssueId(issueId);
  dispatch({ type: DELETE_ISSUE, payload: { projectId, issueId } });
}

const projectsLens = R.lensProp('projects');
const issuesLens = R.lensProp('issues');

const reducer = (state = {}, action) => R.pathOr(
  () => {
      if (action.type.startsWith('@@redux/INIT')) {
        return state;
      } 
      console.error(`no handler for ${action.type}`);
      return state
  },
  [action.type],  
  {
    [LOAD_PROJECTS]: (state, action) => R.set(
      projectsLens,
      R.indexBy(R.prop('projectId'), action.payload),
      state
    ),
    [UPDATE_PROJECT]: (state, action) => R.over(
      projectsLens,
      R.mergeLeft({ [action.payload.projectId]: action.payload }),
      state
    ),
    [DELETE_PROJECT]: (state, { payload: projectId }) => R.over(
      projectsLens,
      R.omit([projectId]),
      state
    ),
    [LOAD_ISSUES]: (state, { payload: { projectId, issues } }) => R.set(
      R.compose(issuesLens, R.lensProp(projectId)),
      R.indexBy(R.prop('issueId'), issues),
      state
    ),
    [UPDATE_ISSUE]: (state, { payload: issue }) => R.over(
      R.compose(issuesLens, R.lensProp(issue.projectId)),
      R.mergeLeft({ [issue.issueId]: issue }),
      state
    ),
    [DELETE_ISSUE]: (state, { payload: { projectId, issueId } }) => R.over(
      R.compose(issuesLens, R.lensProp(projectId)),
      R.omit([issueId]),
      state
    )
  }
)(state, action)

const projectsByIdSelector = createSelector(R.view(projectsLens), R.defaultTo({}));
const issuesByIdSelector = projectId => createSelector(
  R.view(issuesLens),
  R.compose(
    R.defaultTo({}),
    R.prop(projectId),
    R.defaultTo({})
  )
);

export const projectsSelector = createSelector(projectsByIdSelector, R.values);

export const projectSelector = projectId =>
  createSelector(
    projectsByIdSelector,
    R.compose(
      R.defaultTo(null),
      R.prop(projectId)
    )
  );

export const issuesSelector = projectId => createSelector(issuesByIdSelector(projectId), R.values);

export const issueSelector = ({ projectId, issueId }) =>
  createSelector(
    issuesByIdSelector(projectId),
    R.compose(
      R.defaultTo(null),
      R.prop(issueId)
    )
  );

const debugLogReducer = (state, action) => {
  console.log('action:', action);
  const result = reducer(state, action);
  console.log('updated store:', result);
  return result;
}

export const store = createStore(
  debugLogReducer,
  applyMiddleware(thunk.withExtraArgument(api))
);

