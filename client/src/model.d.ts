type ProjectId = string

interface Project {
    projectId: ProjectId,
    name: string,
    description: string
}

type IssueId = string

interface Issue {
    issueId: EntityId,
    projectId: ProjectId,
    attributes: any,
}

type EntityTypeId = string

interface EntityType {
    entityTypeId: EntityTypeId,
    projectId: ProjectId,
    name: string,
    descriptor: any
}

type EntityId = string

interface Entity {
    entityId: EntityId,
    projectId: ProjectId,
    entityTypeId: EntityTypeId
}

type AttributeId = string

interface Attribute {
    attributeId: AttributeId,
    entityId: EntityId,
    name: string,
    value: any
}



type ProjectsById = { [projectId: ProjectId]: Project }
type IssuesById = { [issueId: IssueId]: Issue }

interface RootState {
    projects: ProjectsById,
    issues: IssuesById
}

