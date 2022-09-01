import * as R from 'ramda'
import { applyMiddleware, createStore } from 'redux';
import thunk from 'redux-thunk'
import { createSelector } from 'reselect'
import api from './api'

const LOAD_PROJECTS = 'projects/load'

export const loadProjects = async (dispatch, getState, api) => {
  const projects = await api.getApiV1Projects();
  dispatch({ type: LOAD_PROJECTS, payload: projects })
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
    )
  }
)(state, action)

export const projectsSelector = createSelector(R.view(projects), R.values);

export const store = createStore(
  reducer,
  applyMiddleware(thunk.withExtraArgument(api))
);

