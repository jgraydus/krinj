export declare const selectProjects: (state: RootState) => Array<Project>;
export declare const selectProject: (state: RootState, projectId: ProjectId) => Project | null;
export declare const selectEntitiesForProject: (state: RootState, projectId: ProjectId) => Array<Entity>;
export declare const selectEntity: (state: RootState, entityId: EntityId) => Entity | null;
