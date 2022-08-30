import * as R from 'ramda'
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

const mkRow = issue => <TableRow>{JSON.stringify(issue)}</TableRow>

export default ({ issues }) =>
  <Table>
    {R.map(mkRow, issues)}
  </Table>

