import { type AnyAction } from 'redux'
import * as R from 'ramda'
import * as A from './action-types'

const init: RootState = {
    projects: {},
    entities: {},
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


const entitiesReducer = {
    [A.LOAD_ENTITIES]: (
        state: RootState,
        { payload: { projectId, entities } }: { payload: { projectId : ProjectId, entities: Array<Entity> }}
    ) =>({ 
        ...state,
        entities: R.indexBy(R.prop('entityId'), entities),
    }),
    [A.LOAD_ENTITY]: (state: RootState, { payload: entity }: { payload: Entity }) => ({
        ...state,
        entities: {
            ...state.entities,
            [entity.entityId]: entity,
        },
    }),
    [A.UPDATE_ENTITY]: (state: RootState, { payload: entity }: { payload: Entity }) => ({
        ...state,
        entities: {
            ...state.entities,
            [entity.entityId]: entity,
        },
    }),
    [A.DELETE_ENTITY]: (
        state: RootState,
        { payload: { projectId, entityId } }: { payload: { projectId: ProjectId, entityId: EntityId } }
    ) => ({
        ...state,
        entities: R.omit([entityId], state.entities),
    })
}

const attributesReducer = {
    [A.LOAD_ATTRIBUTE]: (state: RootState, { payload: attribute }: { payload: Attribute }) =>
        R.pipe(
          R.set(R.lensPath(['entities', attribute.entityId, 'attributes', attribute.name]), attribute),
          R.set(R.lensPath(['entities', attribute.entityId, 'attributes', 'modifiedAt']), attribute.modifiedAt)
        )(state)
}

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
      ...entitiesReducer,
      ...attributesReducer
  }
)(state, action)


export const debugLogReducer: any = (state: RootState, action: AnyAction) => {
  console.log('action:', action);
  const result = reducer(state, action);
  console.log('updated store:', result);
  return result;
}

