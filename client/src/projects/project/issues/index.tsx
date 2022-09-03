import { useEffect, useState } from 'react'
import { useDispatch, useSelector } from 'react-redux'
import styled from 'styled-components'

import api from '../../../api'
import Loading from '../../../components/loading'
import { issuesSelector, loadIssues } from '../../../redux'
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
const Content = ({ issues, projectId }) => {
  if (issues == null) {
      return <Loading />;
  }
  if (issues.length === 0) {
      return <NoIssues />;
  }
  return <Table issues={issues} projectId={projectId} />;
}

export default ({ projectId }) => {
  const dispatch = useDispatch();
  const issues = useSelector(issuesSelector(projectId));

  useEffect(() => { dispatch(loadIssues(projectId)); }, []);

  return <Content issues={issues} projectId={projectId} />
}

