import { type AnyAction } from 'redux'
import * as R from 'ramda'
import * as A from './action-types'

const init: RootState = {
    projects: {},
    issues: {},
}

const projectsReducer = {
    [A.LOAD_PROJECTS]: (state: RootState, { payload: projects }: { payload: Array<Project> }) => ({
        ...state,
        projects: R.indexBy(R.prop('projectId'), projects),
    }),
    [A.UPDATE_PROJECT]: (state: RootState, action: AnyAction) => ({
        ...state,
        projects: {
            ...state.projects,
            [action.payload.projectId]: action.payload
        }
    }),
    [A.DELETE_PROJECT]: (state: RootState, { payload: projectId }: { payload: ProjectId }) => ({
        ...state,
        projects: R.omit([projectId], state.projects),
    }),
}

/*
const issuesReducer = {
    [A.LOAD_ISSUES]: (
        state: RootState,
        { payload: { projectId, issues } }: { payload: { projectId : ProjectId, issues: Array<Issue> }}
    ) => R.set(
      R.compose(projectsLens, R.lensProp(projectId), issuesLens),
      R.indexBy(R.prop('issueId'), issues),
      state
    ),
    [A.UPDATE_ISSUE]: (state: RootState, { payload: issue }: { payload: Issue }) => R.over(
      R.compose(projectsLens, R.lensProp(issue.projectId), issuesLens),
      R.mergeLeft({ [issue.issueId]: issue }),
      state
    ),
    [A.DELETE_ISSUE]: (
        state: RootState,
        { payload: { projectId, issueId } }: { payload: { projectId: ProjectId, issueId: IssueId } }
    ) => R.over(
      R.compose(projectsLens, R.lensProp(projectId), issuesLens),
      R.omit([issueId]),
      state
    )
}
*/

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
      ...projectsReducer,
      // ...issuesReducer
  }
)(state, action)


export const debugLogReducer: any = (state: RootState, action: AnyAction) => {
  console.log('action:', action);
  const result = reducer(state, action);
  console.log('updated store:', result);
  return result;
}

