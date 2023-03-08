import * as R from 'ramda'
import axios from 'axios'

const getProjects = (): Promise<Array<Project>> =>
    axios.get('/api/projects')

const getProject = (projectId: ProjectId): Promise<Project> =>
    axios.get(`/api/projects/${projectId}`)

const createProject = (arg: { projectName: string, projectDescription: string }): Promise<Project> =>
    axios.post(`/api/projects`, arg)

const updateProject = (projectId: ProjectId, arg: { projectName: string, projectDescription: string }): Promise<Project> =>
    axios.patch(`/api/projects/${projectId}`, arg)

const deleteProject = (projectId: string): Promise<void> =>
    axios.delete(`/api/projects/${projectId}`)

export default {
    getProjects, getProject, createProject, updateProject, deleteProject
};

