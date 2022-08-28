import { useCallback, useEffect, useState } from 'react'
import ReactModal from 'react-modal'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import api from '../api'
import Loading from '../components/loading'
import NewProject from './new-project'
import Table from './table'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  padding: 5px;
  display: flex;
  flex-flow: column nowrap;
`
const Header = styled.div`
  font-size: 20px;
  height: 40px;
  display: flex;
  align-items: center;
`
const HeaderRow = styled.div`
  display: flex;
  flow-flow: row nowrap;
  justify-content: space-between;
`
const NoProjects = styled.div`
  width: 100%;
  flex-grow: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 30px;
`
const Content = ({ projects }) => {
  if (projects == null) {
      return <Loading />;
  }
  if (projects.length == 0) {
      return <NoProjects>No Projects</NoProjects>;
  }
  return <Table projects={projects} />;
}
          //const project = await api.postApiV1ProjectsCreate();
          //navigate(`/projects/${project.projectId}`);

export default () => {
  const navigate = useNavigate();
  const [projects, setProjects] = useState(null);
  const [newProjectModalIsOpen, setNewProjectModalIsOpen] = useState(false);

  const openModal = useCallback(() => setNewProjectModalIsOpen(true), []);
  const closeModal = useCallback(() => setNewProjectModalIsOpen(false), []);

  useEffect(() => {
    (async () => {
        const projects = await api.getApiV1Projects();
        setProjects(projects);
    })()
  },[])

  return (
    <Root>
      <HeaderRow>
        <Header>Projects</Header>
        <button onClick={openModal}>
          New Project
        </button>
      </HeaderRow>
      <Content projects={projects}/>
      <NewProject isOpen={newProjectModalIsOpen} close={closeModal} />
    </Root>
  )
}

