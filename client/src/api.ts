import * as R from 'ramda'
import axios from 'axios'

const getProjects = (): Promise<Array<Project>> =>
    axios.get('/api/projects')
        .then(x => x.data);

const getProject = (projectId: ProjectId): Promise<Project> =>
    axios.get(`/api/projects/${projectId}`)
        .then(x => x.data);

const createProject = (
    arg: { projectName: string, projectDescription: string }
): Promise<Project> =>
    axios.post(`/api/projects`, { name: arg.projectName, description: arg.projectDescription })
        .then(x => x.data);

const updateProject = (
    projectId: ProjectId,
    arg: { projectName: string, projectDescription: string }
): Promise<Project> =>
    axios.patch(`/api/projects/${projectId}`, { name: arg.projectName, description: arg.projectDescription })
        .then(x => x.data);

const deleteProject = (projectId: string): Promise<void> =>
    axios.delete(`/api/projects/${projectId}`)
        .then(x => x.data);

export default {
    getProjects, getProject, createProject, updateProject, deleteProject
};

