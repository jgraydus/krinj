type ProjectId = string
type ProjectName = string
type ProjectDescription = string

interface Project {
    projectId: ProjectId,
    name: ProjectName,
    description: ProjectDescription,
    entityTypes: Array<EntityType>
}

type EntityTypeId = string
type EntityTypeName = string
type EntityTypeDescriptor = any

interface EntityType {
    entityTypeId: EntityTypeId,
    projectId: ProjectId,
    name: EntityTypeName,
    descriptor: EntityTypeDescriptor
}

type AttributeId = string
type AttributeName = string
type AttributeValue = any

interface Attribute {
    attributeId: AttributeId,
    entityId: EntityId,
    name: AttributeName,
    value: AttributeValue,
    createdAt: string,
    modifiedAt: string | null,
}

type EntityId = string

interface Entity {
    entityId: EntityId,
    projectId: ProjectId,
    entityType: EntityType,
    attributes: { [attributeName: AttributeName]: Attribute },
    createdAt: string,
    modifiedAt: string | null
}

type Issue = Entity
type IssueId = EntityId



type ProjectsById = { [projectId: ProjectId]: Project }
type EntitiesById = { [entityId: EntityId]: Entity }

interface RootState {
    projects: ProjectsById,
    entities: EntitiesById
}

