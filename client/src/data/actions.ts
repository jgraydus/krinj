import * as R from 'ramda'
import * as A from './action-types'
import { store } from './store'

type API = any
type AppAction<T> = (dispatch: (arg: any) => typeof store.dispatch, getState: () => RootState, api: API) => Promise<T>;

// --------------------------------------------------------------------------------------------
// projects

export const loadProjects: AppAction<void>
= async (dispatch, _getState, api) => {
  const projects = await api.getProjects();
  dispatch({ type: A.LOAD_PROJECTS, payload: projects });
}

export const loadProject: (projectId: ProjectId) => AppAction<void>
= projectId => async (dispatch, getState, api) => {
  const project = await api.getProject(projectId);
  dispatch({ type: A.LOAD_PROJECT, payload: project });
}

export const createProject: (
    arg: {
        projectName: string,
        projectDescription: string,
    }
) => AppAction<ProjectId>
= arg => async (dispatch, _getState, api) => {
  const entityTypes = [
      { entityTypeName: "ISSUE", entityTypeDescriptor: {} },
      { entityTypeName: "TASK", entityTypeDescriptor: {} },
  ]
  const project = await api.createProject({ ...arg, entityTypes });
  dispatch({ type: A.LOAD_PROJECT, payload: project });
  return project.projectId
}

export const updateProject: (
    projectId: ProjectId,
    arg: { projectName?: string, projectDescription?: string }
) => AppAction<any>
= (projectId, arg) => async (dispatch, _getState, api) => {
  const project = await api.updateProject(projectId, arg);
  dispatch({ type: A.LOAD_PROJECT, payload: project });
}

export const deleteProject: (projectId: ProjectId) => AppAction<any>
= projectId => async (dispatch, _getState, api) => {
  await api.deleteProject(projectId);
  dispatch({ type: A.DELETE_PROJECT, payload: projectId });
}

// --------------------------------------------------------------------------------------------
// entities

export const loadEntities: (projectId: ProjectId) => AppAction<void>
= projectId => async (dispatch, _getState, api) => {
    const entities = await api.getEntities(projectId);
    dispatch({ type: A.LOAD_ENTITIES, payload: { projectId, entities } });
}

export const loadEntity: (entityId: EntityId) => AppAction<void>
= entityId => async (dispatch, _getState, api) => {
    const entity = await api.getEntity(entityId);
    dispatch({ type: A.LOAD_ENTITY, payload: entity });
}

export const createEntity: (
    projectId: ProjectId,
    args: {
        entityTypeId: EntityTypeId,
        attributes: { [attributeName: string]: any }
    }
) => AppAction<EntityId>
= (projectId, args) => async (dispatch, _getState, api) => {
    const attributes = R.map(
        ([ attributeName, attributeValue ]) => ({ attributeName, attributeValue }),
        R.toPairs(args.attributes)
    );
    const entity = await api.createEntity(
        projectId,
        { entityTypeId: args.entityTypeId, attributes }
    );
    dispatch({ type: A.LOAD_ENTITY, payload: entity });
    return entity.entityId
}

export const updateEntity: (entityId: EntityId, updates: any) => AppAction<void>
= (entityId, update) => async (_dispatch, _getState, _api) => {
    // TODO
}

export const deleteEntity: (entityId: EntityId) => AppAction<void>
= entityId => async (_dipatch, _getState, _api) => {
    // TODO
}

// --------------------------------------------------------------------------------------------
// attributes

const loadAttributes = ""

const loadAttribute = ""

const createAttribute = ""

export const updateAttribute: (
    entityId: EntityId,
    args: { attributeName: AttributeName, attributeValue: AttributeValue }
) => AppAction<void>
= (entityId, args) => async (dispatch, _getState, api) => {
    const attribute = await api.updateAttribute(entityId, args);
    dispatch({ type: A.LOAD_ATTRIBUTE, payload: attribute });
}

const deleteAttribute = ""

