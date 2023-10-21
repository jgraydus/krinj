import { type AnyAction } from 'redux'
import * as R from 'ramda'
import * as A from './action-types'

const init: RootState = {
    projects: {},
    entities: {},
}

const projectsLens = R.lensProp<RootState, 'projects'>('projects');
const indexByProjectId = (projects: Array<Project>) => R.indexBy(R.prop('projectId'), projects)

const projectsReducer = {
    [A.LOAD_PROJECTS]: (state: RootState, { payload: projects }: { payload: Array<Project> }) =>
        R.set(
            projectsLens,
            indexByProjectId(projects),
            state
        ),
    [A.LOAD_PROJECT]: (state: RootState, action: AnyAction) =>
        R.over(
            projectsLens,
            R.mergeLeft({ [action['payload'].projectId]: action['payload'] }),
            state
        ),
    [A.DELETE_PROJECT]: (state: RootState, { payload: projectId }: { payload: ProjectId }) =>
        R.over(
            projectsLens,
            R.omit([projectId]),
            state
        ),
}

const entitiesLens = R.lensProp<RootState, 'entities'>('entities');
const indexByEntityId = (entities: Array<Entity>) => R.indexBy(R.prop('entityId'), entities)

const entitiesReducer = {
    [A.LOAD_ENTITIES]: (
        state: RootState,
        { payload: { entities } }: { payload: { entities: Array<Entity> }}
    ) => R.set(
             entitiesLens,
             indexByEntityId(entities),
             state
         ),
    [A.LOAD_ENTITY]: (state: RootState, { payload: entity }: { payload: Entity }) =>
        R.over(
            entitiesLens,
            R.mergeLeft({ [entity.entityId]: entity }),
            state
        ),
    [A.DELETE_ENTITY]: (
        state: RootState,
        { payload: { entityId } }: { payload: { entityId: EntityId } }
    ) => R.over(
             entitiesLens,
             R.omit([entityId]),
             state
         ),
}

const attributesReducer = {
    [A.LOAD_ATTRIBUTE]: (state: RootState, { payload: attribute }: { payload: Attribute }) =>
        R.pipe(
          R.set(R.lensPath(['entities', attribute.entityId, 'attributes', attribute.name]), attribute.value),
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

