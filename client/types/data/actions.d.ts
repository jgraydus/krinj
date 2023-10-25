import { store } from './store';
type API = any;
type AppAction<T> = (dispatch: (arg: any) => typeof store.dispatch, getState: () => RootState, api: API) => Promise<T>;
export declare const loadProjects: AppAction<void>;
export declare const loadProject: (projectId: ProjectId) => AppAction<void>;
export declare const createProject: (arg: {
    projectName: string;
    projectDescription: string;
}) => AppAction<ProjectId>;
export declare const updateProject: (projectId: ProjectId, arg: {
    projectName?: string;
    projectDescription?: string;
}) => AppAction<any>;
export declare const deleteProject: (projectId: ProjectId) => AppAction<any>;
export declare const loadEntities: (projectId: ProjectId) => AppAction<void>;
export declare const loadEntity: (entityId: EntityId) => AppAction<void>;
export declare const createEntity: (projectId: ProjectId, args: {
    entityTypeId: EntityTypeId;
    attributes: {
        [attributeName: string]: any;
    };
}) => AppAction<EntityId>;
export declare const updateEntity: (entityId: EntityId, updates: any) => AppAction<void>;
export declare const deleteEntity: (entityId: EntityId) => AppAction<void>;
export declare const loadAttributes = "";
export declare const loadAttribute = "";
export declare const createAttribute = "";
export declare const updateAttribute: (entityId: EntityId, args: {
    attributeName: AttributeName;
    attributeValue: AttributeValue;
}) => AppAction<void>;
export declare const deleteAttribute = "";
export declare const logIn: (args: {
    emailAddress: string;
    password: string;
}) => AppAction<string | null>;
export declare const logOut: () => AppAction<void>;
export declare const me: () => AppAction<void>;
export declare const showLogInView: () => AppAction<void>;
export declare const hideLogInView: () => AppAction<void>;
export {};
