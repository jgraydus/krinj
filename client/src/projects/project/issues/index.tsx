import { useEffect, useState } from 'react'
import styled from 'styled-components'
import api from '../../../api'
import Loading from '../../../components/loading'
import Table from './table'

const NoIssues = styled(({ className }) => <div className={className}>No Issues</div>)`
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
      return <NoIssues />;
  }
  return <Table issues={issues} />;
}

export default ({ projectId }) => {
  const [issues, setIssues] = useState(null);

  useEffect(() => {
    (async () => {
      const issues = await api.getApiV1Issues(projectId, null);
      setIssues(issues);
    })()
  }, [projectId]);

  return <Content issues={issues} />
}

