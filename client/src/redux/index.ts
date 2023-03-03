import * as R from 'ramda'
import { AnyAction, applyMiddleware, createStore } from 'redux';
import thunk from 'redux-thunk'
import api from '../api'
import { DELETE_ISSUE, DELETE_PROJECT, LOAD_ISSUES, LOAD_PROJECTS, UPDATE_ISSUE, UPDATE_PROJECT } from './action-types'

export type ProjectsById = { [index: ProjectId]: ProjectWithIssues }
export type IssuesById = { [index: IssueId]: Issue }

export interface RootState {
    projects: ProjectsById
}

export const projectsLens: R.Lens<RootState, ProjectsById> = R.lensProp('projects');
export const issuesLens: R.Lens<Project, IssuesById> = R.lensProp('issues');

const init: RootState = { projects: {} }

const reducer = (state: RootState = init, action: AnyAction) => R.pathOr(
  (state: RootState, action: AnyAction) => {
      if (action.type.startsWith('@@redux/INIT')) {
        return state;
      } 
      console.error(`no handler for ${action.type}`);
      return state
  },
  [action.type],  
  {
    [LOAD_PROJECTS]: (state: RootState, { payload: projects }: { payload: Array<Project> }) => R.set(
      projectsLens,
      R.indexBy(R.prop('projectId'), projects),
      state
    ),
    [UPDATE_PROJECT]: (state: RootState, action: AnyAction) => R.over(
      projectsLens,
      R.mergeLeft({ [action.payload.projectId]: action.payload }),
      state
    ),
    [DELETE_PROJECT]: (state: RootState, { payload: projectId }: { payload: ProjectId }) => R.over(
      projectsLens,
      R.omit([projectId]),
      state
    ),
    [LOAD_ISSUES]: (
        state: RootState,
        { payload: { projectId, issues } }: { payload: { projectId : ProjectId, issues: Array<Issue> }}
    ) => R.set(
      R.compose(projectsLens, R.lensProp(projectId), issuesLens),
      R.indexBy(R.prop('issueId'), issues),
      state
    ),
    [UPDATE_ISSUE]: (state: RootState, { payload: issue }: { payload: Issue }) => R.over(
      R.compose(projectsLens, R.lensProp(issue.projectId), issuesLens),
      R.mergeLeft({ [issue.issueId]: issue }),
      state
    ),
    [DELETE_ISSUE]: (
        state: RootState,
        { payload: { projectId, issueId } }: { payload: { projectId: ProjectId, issueId: IssueId } }
    ) => R.over(
      R.compose(projectsLens, R.lensProp(projectId), issuesLens),
      R.omit([issueId]),
      state
    )
  }
)(state, action)


const debugLogReducer: any = (state: RootState, action: AnyAction) => {
  console.log('action:', action);
  const result = reducer(state, action);
  console.log('updated store:', result);
  return result;
}

export const store = createStore(
  debugLogReducer,
  applyMiddleware(thunk.withExtraArgument(api))
);

