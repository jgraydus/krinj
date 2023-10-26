import { useCallback, useState } from 'react'
import styled from 'styled-components'
import { Button } from 'components'
import IssuesTable from './issues'
import NewIssueModal from './issues/new-issue'

const Root = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  height: 100%;
  width: 100%;
`
const NewIssueButton = styled(({ className, onClick }) => 
  <div className={className}>
    <Button onClick={onClick}>New Issue</Button>
  </div>)`
 display: flex;
 flex-flow: row-reverse nowrap;
 padding: 5px;
`

export default ({ project }: { project: Project }) => {
  const [newIssueModalIsOpen, setNewIssueModalIsOpen] = useState(false);

  const closeNewIssueModal = useCallback(() => setNewIssueModalIsOpen(false), [setNewIssueModalIsOpen]);
  const openNewIssueModal = useCallback(() => setNewIssueModalIsOpen(true), [setNewIssueModalIsOpen]);

  return (
    <Root>
      <NewIssueButton onClick={openNewIssueModal} />
      <IssuesTable projectId={project.projectId} />
      <NewIssueModal
        close={closeNewIssueModal}
        isOpen={newIssueModalIsOpen}
        project={project}
      />
    </Root>
  )
}

