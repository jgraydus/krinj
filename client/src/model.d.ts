type ProjectId = string

interface Project {
    projectId: ProjectId,
    name: string,
    description: string
}

type EntityTypeId = string

interface EntityType {
    entityTypeId: EntityTypeId,
    projectId: ProjectId,
    name: string,
    descriptor: any
}

type AttributeId = string

interface Attribute {
    attributeId: AttributeId,
    entityId: EntityId,
    name: string,
    value: any
}

type EntityId = string

interface Entity {
    entityId: EntityId,
    projectId: ProjectId,
    entityType: EntityType,
    attributes: { [attributeName: string]: Attribute }
}

type Issue = Entity
type IssueId = EntityId



type ProjectsById = { [projectId: ProjectId]: Project }
type EntitiesById = { [entityId: EntityId]: Entity }

interface RootState {
    projects: ProjectsById,
    entities: EntitiesById
}

