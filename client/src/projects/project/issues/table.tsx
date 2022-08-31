import * as R from 'ramda'
import { useCallback } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'

const Table = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
  border: 1px solid red;
`
const TableRow = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: row nowrap;
  height: 30px;
  width: 100%;
  border: 1px solid black;
  font-size: 8px;
`

const Row = ({ issue, onClick }) =>
  <TableRow onClick={onClick}>
    {JSON.stringify(issue)}
  </TableRow>

export default ({ issues, projectId }) => {
  const navigate = useNavigate();

  return (
    <Table>
      {R.map(
        issue =>
          <Row
            key={issue.issueId}
            issue={issue}
            onClick={() => navigate(`/projects/${projectId}/issues/${issue.issueId}`)}
          />,
        issues
      )}
    </Table>
  )
}

