import * as R from 'ramda'
import { useCallback } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'

const Table = styled.div`
  box-sizing: border-box;
  padding: 0 5px 0 5px;
  display: flex;
  flex-flow: column nowrap;
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
  & > div:last-child {
    border-bottom: 1px solid black;
  }
`
const HeaderRow = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: row nowrap;
  height: 30px;
  width: 100%;
  font-size: 8px;
  border-left: 1px solid black;
  border-right: 1px solid black;
  border-top: 1px solid black;
`
const TableRow = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: row nowrap;
  height: 30px;
  width: 100%;
  font-size: 8px;
  border-left: 1px solid black;
  border-right: 1px solid black;
  border-top: 1px solid black;
  cursor: pointer;
  :hover {
    background-color: rgb(255,255,255,0.5);
  }
`
const Cell = styled.div<{ width: string }>`
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  justify-content: flex-start;
  padding: 5px;
  width: ${props => props.width};
`

const Row = ({ issue, onClick }: { issue: Entity, onClick: any }) =>
  <TableRow onClick={onClick}>
    <Cell width="30%">{R.pathOr('--', ['attributes', 'name'], issue)}</Cell>
    <Cell width="20%">{R.pathOr('--', ['attributes', 'owner'], issue)}</Cell>
    <Cell width="20%">{R.pathOr('No assignee', ['attributes', 'assignee'], issue)}</Cell>
    <Cell width="10%">{R.pathOr('--', ['attributes', 'state'], issue)}</Cell>
    <Cell width="10%">{R.pathOr('--', ['attributes', 'createdAt'], issue)}</Cell>
    <Cell width="10%">{R.pathOr('--', ['attributes', 'updatedAt'], issue)}</Cell>
  </TableRow>

export default ({ issues, projectId }: { issues: Array<Issue>, projectId: ProjectId }) => {
  const navigate = useNavigate();

  return (
    <Table>
      <HeaderRow>
        <Cell width="30%">Name</Cell>
        <Cell width="20%">Owner</Cell>
        <Cell width="20%">Assignee</Cell>
        <Cell width="10%">State</Cell>
        <Cell width="10%">Created</Cell>
        <Cell width="10%">Last Updated</Cell>
      </HeaderRow>
      {R.map(
        issue =>
          <Row
            key={issue.entityId}
            issue={issue}
            onClick={() => navigate(`/projects/${projectId}/issues/${issue.entityId}`)}
          />,
        issues
      )}
    </Table>
  )
}

