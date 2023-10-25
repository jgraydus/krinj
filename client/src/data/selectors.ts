import * as R from 'ramda'
import { createSelector } from 'reselect'

const selectProjectsById: (state: RootState) => ProjectsById
= createSelector(
    (state: RootState) => state.projects,
    (projectsById: ProjectsById) => R.defaultTo({}, projectsById)
);

export const selectProjects: (state: RootState) => Array<Project>
= createSelector(
    selectProjectsById,
    (projectsById: ProjectsById) => R.values(projectsById)
);

export const selectProject: (state: RootState, projectId: ProjectId) => Project | null
= createSelector(
    [
        selectProjectsById,
        (_state: RootState, projectId: ProjectId) => projectId
    ],
    (projects: ProjectsById, projectId) => projects[projectId] || null
);

const selectEntitiesById: (state: RootState) => EntitiesById
= createSelector(
    (state: RootState) => state.entities,
    (entitiesById: EntitiesById) => R.defaultTo({}, entitiesById)
);

const selectAllEntities: (state: RootState) => Array<Entity>
= createSelector(
    (state: RootState) => state.entities,
    (entitiesById: EntitiesById) => R.values(entitiesById)
);

type EntitiesByProjectId = { [projectId: ProjectId]: Array<Entity> }

const selectEntitiesByProjectId: (state: RootState) => EntitiesByProjectId
= createSelector(
    selectAllEntities,
    (entities: Array<Entity>) => R.groupBy(entity => entity.projectId, entities)
);

export const selectEntitiesForProject: (state: RootState, projectId: ProjectId) => Array<Entity>
= createSelector(
    [
        selectEntitiesByProjectId,
        (_state: RootState, projectId: ProjectId) => projectId
    ],
    (entitiesByProjectId: EntitiesByProjectId, projectId: ProjectId) => entitiesByProjectId[projectId] || []
);

export const selectEntity: (state: RootState, entityId: EntityId) => Entity | null
= createSelector(
    [
        selectEntitiesById,
        (_state: RootState, entityId: EntityId) => entityId
    ],
    (entitiesById: EntitiesById, entityId: EntityId) => entitiesById[entityId] || null
);

export const showLogInViewSelector: (state: RootState) => boolean
= createSelector(
   (state: RootState) => state.views,
   (views: ViewsState) => views.login
);

export const meSelector: (state: RootState) => User | null
= state => state.me

