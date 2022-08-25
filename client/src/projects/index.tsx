import * as R from 'ramda'
import { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import api from '../api'

const Root = styled.div`
  padding: 5px;
  border: 1px solid purple;
`
const Header = styled.div`
  font-size: 20px;
`
const HeaderRow = styled.div`
  display: flex;
  flow-flow: row nowrap;
  justify-content: space-between;
`
const TableRow = ({ project }) => {

  return (
    <div>
      <div>{project.projectId}</div>
      <div>{project.title}</div>
    </div>
  )
}

export default () => {
  const navigate = useNavigate();
  const [projects, setProjects] = useState('');

  useEffect(() => {
    (async () => {
        const projects = await api.getApiV1Projects();
        console.log(projects);
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
      {R.addIndex(R.map)((project, i) => <TableRow key={i} project={project} />, (projects || []))}
    </Root>
  )
}

