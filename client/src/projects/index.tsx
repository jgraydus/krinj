import { useCallback, useEffect, useState } from 'react'
import styled from 'styled-components'
import { Button, Loading } from 'components'
import NewProject from './new-project'
import Table from './table'
import { loadProjects, selectProjects, useDispatch, useSelector } from 'data'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  padding: 5px;
  display: flex;
  flex-flow: column nowrap;
`
const Header = styled.div`
  font-size: 20px;
  height: 40px;
  display: flex;
  align-items: center;
`
const HeaderRow = styled.div`
  display: flex;
  flow-flow: row nowrap;
  justify-content: space-between;
`
const NoProjects = styled(({ className }) =>
  <div className={className}>No Projects</div>
)`
  width: 100%;
  flex-grow: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 30px;
`
const Content = ({ projects }: { projects: Array<Project> }) => {
  if (projects == null) {
      return <Loading />;
  }
  if (projects.length == 0) {
      return <NoProjects />;
  }
  return <Table projects={projects} />;
}

const Projects = ({
    closeModal,
    modalIsOpen,
    openModal,
    projects
    }: {
        closeModal: () => void,
        modalIsOpen: boolean,
        openModal: () => void,
        projects: Array<Project>
    }) =>
  <Root>
    <HeaderRow>
      <Header>Projects</Header>
      <Button onClick={openModal}>
        New Project
      </Button>
    </HeaderRow>
    <Content projects={projects}/>
    <NewProject isOpen={modalIsOpen} close={closeModal} />
  </Root>

export default () => {
  const dispatch = useDispatch();
  const projects = useSelector(selectProjects);
  useEffect(() => { dispatch(loadProjects) }, []);

  const [modalIsOpen, setModalIsOpen] = useState(false);
  const openModal = useCallback(() => setModalIsOpen(true), []);
  const closeModal = useCallback(() => setModalIsOpen(false), []);

  return (
    <Projects
      closeModal={closeModal}
      modalIsOpen={modalIsOpen}
      openModal={openModal}
      projects={projects}
    />
  )
}
