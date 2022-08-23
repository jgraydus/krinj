import styled from 'styled-components'
import { useNavigate } from 'react-router-dom'
import api from './api'

const Root = styled.div`
  padding: 5px;
  border: 1px solid purple;
`
const HeaderRow = styled.div`
  display: flex;
  flow-flow: row nowrap;
  justify-content: space-between;
`

export default () => {
  const navigate = useNavigate();

  return (
    <Root>
      <HeaderRow>
        <div>Projects</div>
        <button onClick={async () => {
          const project = await api.postApiV1ProjectsCreate();
          navigate(`/projects/${project.projectId}`);
        }}>
          New Project
        </button>
      </HeaderRow>
    </Root>
  )
}

