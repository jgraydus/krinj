import { useCallback, useEffect, useState } from 'react'
import styled from 'styled-components'

import api from '../../api'
import Loading from '../../components/loading'
import Issues from './issues'

const Root = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  height: 100%;
  width: 100%;
  padding: 5px;
  border: 1px solid red;
`
const NoIssues = styled(({ className }) => <div className={className}>No Issues</div>)`
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 20px;
`
const Table = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  width: 100%;
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
  border: 1px solid purple;
`

const createNewIssue = async projectId => {
  const issue = await api.postApiV1IssuesCreate(JSON.stringify(projectId));
  await api.patchApiV1IssuesByIssueId(issue.issueId, [{
    tag: 'Title', contents: 'ISSUE A'
  }]);
  return issue;
}

export default ({ projectId }) => {
  const newIssue = useCallback(async () => {
    const issue = await createNewIssue(projectId);
    setIssues([iissue, ...issues]);
  }, [projectId]);

  return (
    <Root>
      <button onClick={newIssue}>New Issue</button>
      <Issues projectId={projectId} />
    </Root>
  )
}

