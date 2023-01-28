import * as R from 'ramda'
import { createSelector } from 'reselect'
import { IssuesById, issuesLens, ProjectsById, projectsLens } from './'

const projectsByIdSelector =
    createSelector(
        [R.view(projectsLens)],
        R.defaultTo({})
    );

export const projectsSelector =
    createSelector(
        [projectsByIdSelector],
        R.values
    );

export const projectSelector =
    (projectId: ProjectId) => createSelector(
        [projectsByIdSelector],
        (projects: ProjectsById) => projects[projectId] || null
    );

const issuesByIdSelector =
    (projectId: ProjectId) => createSelector(
        [projectSelector(projectId)],
        (project: Project) => project.issues
    );


export const issuesSelector =
    (projectId: ProjectId) => createSelector(
        [issuesByIdSelector(projectId)],
        R.values
    );

export const issueSelector =
    ({ projectId, issueId }: { projectId: ProjectId, issueId: IssueId }) => createSelector(
        [issuesByIdSelector(projectId)],
        (issues: IssuesById) => issues[issueId] || null
    );

