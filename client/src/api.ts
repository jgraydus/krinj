import * as R from 'ramda'
import axios from 'axios'

// -------------------------------------------------------------------------------------------
// projects

const getProjects = (): Promise<Array<Project>> =>
    axios.get('/api/projects')
        .then(x => x.data);

const getProject = (projectId: ProjectId): Promise<Project> =>
    axios.get(`/api/projects/${projectId}`)
        .then(x => x.data);

const createProject = (
    arg: {
        projectName: string,
        projectDescription: string,
        entityTypes: Array<{ entityTypeName: EntityTypeName, entityTypeDescriptor: EntityTypeDescriptor }>
    }
): Promise<Project> =>
    axios.post(
        `/api/projects`,
        {
            name: arg.projectName,
            description: arg.projectDescription,
            entityTypes: R.map(
                ({ entityTypeName, entityTypeDescriptor}) => [entityTypeName, entityTypeDescriptor],
                arg.entityTypes
            )
        }
    ).then(x => x.data);

const updateProject = (
    projectId: ProjectId,
    arg: { projectName: string, projectDescription: string }
): Promise<Project> =>
    axios.patch(`/api/projects/${projectId}`, { name: arg.projectName, description: arg.projectDescription })
        .then(x => x.data);

const deleteProject = (projectId: string): Promise<void> =>
    axios.delete(`/api/projects/${projectId}`)
        .then(x => x.data);

// -------------------------------------------------------------------------------------------
// entity types

const getEntityTypes = (): Promise<Array<EntityType>> =>
    axios.get('/api/entity-types')
        .then(x => x.data);

const getEntityType = (entityTypeId: EntityTypeId): Promise<EntityType> =>
    axios.get(`/api/entity-types/${entityTypeId}`)
        .then(x => x.data);

const createEntityType = (
    arg: { projectId: ProjectId, entityTypeName: string, entityTypeDescriptor: any }
): Promise<EntityType> =>
    axios.post(
        '/api/entity-types',
        { projectId: arg.projectId, name: arg.entityTypeName, descriptor: arg.entityTypeDescriptor }
    ).then(x => x.data);

const updateEntityType = (
    entityTypeId: EntityTypeId,
    arg: { entityTypeName?: string, entityTypeDescriptor?: any }
): Promise<EntityType> =>
    axios.patch(
        `/api/entity-types/${entityTypeId}`,
        { name: arg.entityTypeName, descriptor: arg.entityTypeDescriptor }
    ).then(x => x.data);

const deleteEntityType = (entityTypeId: EntityTypeId): Promise<void> =>
    axios.delete(`/api/entity-types/${entityTypeId}`)
        .then(x => x.data);

// -------------------------------------------------------------------------------------------
// entities

const getEntities = (projectId: ProjectId): Promise<Array<Entity>> =>
    axios.get(`/api/entities?projectId=${projectId}`)
        .then(x => x.data);

const getEntity = (entityId: EntityId): Promise<Entity> =>
    axios.get(`/api/entities/${entityId}`)
        .then(x => x.data);

const createEntity = (
    projectId: ProjectId,
    arg: {
        entityTypeId: EntityTypeId,
        attributes: Array<{ attributeName: AttributeName, attributeValue: AttributeValue }>
    }
): Promise<Entity> =>
    axios.post('/api/entities', {
        projectId,
        entityTypeId: arg.entityTypeId,
        attributes: R.map(
            ({ attributeName, attributeValue }) => [attributeName, attributeValue],
            arg.attributes
        )
    }).then(x => x.data);

const updateEntity = (
    entityId: EntityId,
    arg: { projectId: ProjectId, entityTypeId: EntityTypeId }
): Promise<Entity> =>
    axios.patch(`/api/entities/${entityId}`, arg)
        .then(x => x.data);

const deleteEntity = (entityId: EntityId): Promise<void> =>
    axios.delete(`/api/entities/${entityId}`)
        .then(x => x.data);

// -------------------------------------------------------------------------------------------
// attributes

const getAttributes = (entityId: EntityId): Promise<Array<Attribute>> =>
    axios
        .get(`/api/entities/${entityId}/attributes`)
        .then(x => x.data)

const getAttribute = (entityId: EntityId, attributeName: AttributeName): Promise<Attribute> =>
    axios
        .get(`/api/entities/${entityId}/attributes?name=${attributeName}`)
        .then(x => x.data);

const createAttribute = (
    entityId: EntityId,
    arg: { attributeName: string, attributeValue: any }
): Promise<Attribute> =>
    axios
        .post(
            `/api/entities/${entityId}/attributes`, 
            [[arg.attributeName, arg.attributeValue]]
        )
        .then(x => x.data);

const updateAttribute = (
    entityId: EntityId,
    arg: { attributeName: string, attributeValue: any }
): Promise<Attribute> =>
    axios
        .patch(
            `/api/entities/${entityId}/attributes`, 
            [[arg.attributeName, arg.attributeValue]]
        )
        .then(x => x.data[0]);

const deleteAttribute = (entityId: EntityId, attributeName: AttributeName): Promise<void> =>
    axios
        .delete(`/api/entities/${entityId}/attributes?name=${attributeName}`)
        .then(x => x.data);

const logIn = (emailAddress: string, password: string): Promise<string | null> =>
    axios
        .post('/api/login', { emailAddress, password })
        .then(() => null)
        .catch(e => e.toString());

const logOut = (): Promise<void> =>
    axios
        .post('/api/logout');

const me = (): Promise<User> =>
    axios
        .get('/api/me')
        .then(x => x.data)
        .catch(() => null);

export default {
    getAttributes, getAttribute, createAttribute, updateAttribute, deleteAttribute,
    getEntities, getEntity, createEntity, updateEntity, deleteEntity,
    getEntityType, getEntityTypes, createEntityType, updateEntityType, deleteEntityType,
    getProjects, getProject, createProject, updateProject, deleteProject,
    logIn, logOut, me
};

