import { useEffect, useState } from 'react'
import styled from 'styled-components'
import api from '../api'
import Loading from '../components/loading'
import Table from './table'

const Root = styled.div`
  width: 100%;
  height: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Header = styled.div`
  font-size: 20px;
  height: 40px;
  display: flex;
  align-items: center;
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
`
const NoIssues = styled.div`
  width: 100%;
  height: 100%;
  font-size: 30px;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  justify-content: center;
`
const Content = ({ issues }) => {
  if (issues == null) {
      return <Loading />;
  }
  if (issues.length === 0) {
      return <NoIssues>No Issues</NoIssues>;
  }
  return <Table issues={issues} />;
}

export default ({ projectId }) => {
  const [issues, setIssues] = useState(null);

  useEffect(() => {
    (async () => {
        const issues = await api.getApiV1Issues(projectId);
        setIssues(issues);
    })()
  }, [projectId]);

  return (
    <Root>
      <Header>Issues</Header>
      <Content issues={issues} />
    </Root>
  )
}
