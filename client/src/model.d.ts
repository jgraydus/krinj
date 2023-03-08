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






type ProjectsById = { [projectId: ProjectId]: Project }
type IssuesById = { [issueId: IssueId]: Issue }

interface RootState {
    projects: ProjectsById,
    issues: IssuesById
}

