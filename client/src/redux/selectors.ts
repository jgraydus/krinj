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
        (state: RootState, projectId: ProjectId) => projectId
    ],
    (projects: ProjectsById, projectId) => projects[projectId] || null
);

const selectIssuesById: (state: RootState) => IssuesById
= createSelector(
    (state: RootState) => state.issues,
    (issuesById: IssuesById) => R.defaultTo({}, issuesById)
);

const selectAllIssues: (state: RootState) => Array<Issue>
= createSelector(
    (state: RootState) => state.issues,
    (issuesById: IssuesById) => R.values(issuesById)
);

type IssuesByProjectId = { [projectId: ProjectId]: Array<Issue> }

const selectIssuesByProjectId: (state: RootState) => IssuesByProjectId
= createSelector(
    selectAllIssues,
    (issues: Array<Issue>) => R.groupBy(issue => issue.projectId, issues)
);

export const selectIssuesForProject: (state: RootState, projectId: ProjectId) => Array<Issue>
= createSelector(
    [
        selectIssuesByProjectId,
        (_state: RootState, projectId: ProjectId) => projectId
    ],
    (issuesByProjectId: IssuesByProjectId, projectId: ProjectId) => issuesByProjectId[projectId] || []
);

export const selectIssue: (state: RootState, issueId: IssueId) => Issue | null
= createSelector(
    [
        selectIssuesById,
        (_state: RootState, issueId: IssueId) => issueId
    ],
    (issuesById: IssuesById, issueId: IssueId) => issuesById[issueId] || null
);

