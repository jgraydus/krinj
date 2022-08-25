import { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import api from '../api'
import Loading from '../components/loading'
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

export default () => {
  const navigate = useNavigate();
  const [projects, setProjects] = useState(null);

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
        <button onClick={async () => {
          const project = await api.postApiV1ProjectsCreate();
          navigate(`/projects/${project.projectId}`);
        }}>
          New Project
        </button>
      </HeaderRow>
      <Content projects={projects}/>
    </Root>
  )
}

