import * as R from 'ramda'
import { applyMiddleware, createStore } from 'redux';
import thunk from 'redux-thunk'
import { createSelector } from 'reselect'
import api from './api'

const LOAD_PROJECTS = 'projects/load'
const UPDATE_PROJECT = 'projects/update'

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

const projects = R.lensProp('projects')

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
        projects,
        R.indexBy(R.prop('projectId'), action.payload),
        state
    ),
    [UPDATE_PROJECT]: (state, action) => R.over(
        projects,
        R.mergeLeft({ [action.payload.projectId]: action.payload }),
        state
    )
  }
)(state, action)

const projectsByIdSelector = createSelector(R.view(projects), R.defaultTo({}));

export const projectsSelector = createSelector(projectsByIdSelector, R.values);

export const projectSelector = projectId =>
  createSelector(
    projectsByIdSelector,
    R.compose(
      R.defaultTo(null),
      R.prop(projectId)
    )
  )

export const store = createStore(
  reducer,
  applyMiddleware(thunk.withExtraArgument(api))
);

