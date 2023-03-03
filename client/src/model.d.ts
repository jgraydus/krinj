/*
type ProjectId = string
type IssueId = string

interface Issue {
    issueId: IssueId,
    projectId: ProjectId,
    title?: string,
    description?: string,
    owner?: string
    assignee?: string,
    state?: string,
    createdAt: string,
    updatedAt: string
}

interface ProjectAndIssues {
    projectId: ProjectId,
    title?: string,
    description?: string,
    issues: { [index: IssueId]: Issue }
}
*/

type IssueId = string
type ProjectId = string

type ProjectWithIssues = Project & { issues: { [index: IssueId]: Issue } }
