import * as R from 'ramda'
import { ReactNode } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'

const Root = styled.div`
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
  overflow-y: scroll;
  & > div:last-child {
    border-bottom: 1px solid black;
  }
`
const _TableRow = styled.div`
  height: 30px;
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  border-top: 1px solid black;
  border-left: 1px solid black;
  border-right: 1px solid black;
  border-bottom: 0;
  :hover {
    background-color: rgba(255,255,255,0.5);
    cursor: pointer;
  }
`
const ProjectName = styled.div`
  font-size: 14px;
  flex-grow: 1;
  padding: 10px;
`
const ProjectId = styled.div`
  font-size: 9px;
  padding 10px;
`

const TableRow = ({ project }: { project: Project }) => {
  const navigate = useNavigate();
  
  return (
    <_TableRow onClick={() => navigate(`/projects/${project.projectId}`)}>
      <ProjectName>{project.title}</ProjectName>
      <ProjectId>{project.projectId}</ProjectId>
    </_TableRow>
  )
}

const mkTableRow: any = (prj: Project, i: number): ReactNode => <TableRow key={i} project={prj} />

const mkTableRows: any = (projects: Array<Project>) => R.addIndex(R.map)(mkTableRow, projects)

export default ({ projects }: { projects: Array<Project> }) =>
  <Root>
    {mkTableRows(projects)}
  </Root>

