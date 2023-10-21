export declare const store: import("redux").Store<{}, import("redux").Action<any>> & {
    dispatch: import("redux-thunk").ThunkDispatch<any, {
        getAttributes: (entityId: string) => Promise<Attribute[]>;
        getAttribute: (entityId: string, attributeName: string) => Promise<Attribute>;
        createAttribute: (entityId: string, arg: {
            attributeName: string;
            attributeValue: any;
        }) => Promise<Attribute>;
        updateAttribute: (entityId: string, arg: {
            attributeName: string;
            attributeValue: any;
        }) => Promise<Attribute>;
        deleteAttribute: (entityId: string, attributeName: string) => Promise<void>;
        getEntities: (projectId: string) => Promise<Entity[]>;
        getEntity: (entityId: string) => Promise<Entity>;
        createEntity: (projectId: string, arg: {
            entityTypeId: string;
            attributes: {
                attributeName: string;
                attributeValue: any;
            }[];
        }) => Promise<Entity>;
        updateEntity: (entityId: string, arg: {
            projectId: string;
            entityTypeId: string;
        }) => Promise<Entity>;
        deleteEntity: (entityId: string) => Promise<void>;
        getEntityType: (entityTypeId: string) => Promise<EntityType>;
        getEntityTypes: () => Promise<EntityType[]>;
        createEntityType: (arg: {
            projectId: string;
            entityTypeName: string;
            entityTypeDescriptor: any;
        }) => Promise<EntityType>;
        updateEntityType: (entityTypeId: string, arg: {
            entityTypeName?: string;
            entityTypeDescriptor?: any;
        }) => Promise<EntityType>;
        deleteEntityType: (entityTypeId: string) => Promise<void>;
        getProjects: () => Promise<Project[]>;
        getProject: (projectId: string) => Promise<Project>;
        createProject: (arg: {
            projectName: string;
            projectDescription: string;
            entityTypes: {
                entityTypeName: string;
                entityTypeDescriptor: any;
            }[];
        }) => Promise<Project>;
        updateProject: (projectId: string, arg: {
            projectName: string;
            projectDescription: string;
        }) => Promise<Project>;
        deleteProject: (projectId: string) => Promise<void>;
    }, import("redux").AnyAction>;
};
