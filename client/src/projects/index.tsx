import { useCallback, useEffect, useState } from 'react'
import ReactModal from 'react-modal'
import { useDispatch, useSelector } from 'react-redux'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import api from '../api'
import Loading from '../components/loading'
import NewProject from './new-project'
import Table from './table'
import { loadProjects, projectsSelector } from '../redux'

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
const Content = ({ projects }) => {
  if (projects == null) {
      return <Loading />;
  }
  if (projects.length == 0) {
      return <NoProjects />;
  }
  return <Table projects={projects} />;
}

const Projects = ({ closeModal, modalIsOpen, openModal, projects }) =>
  <Root>
    <HeaderRow>
      <Header>Projects</Header>
      <button onClick={openModal}>
        New Project
      </button>
    </HeaderRow>
    <Content projects={projects}/>
    <NewProject isOpen={modalIsOpen} close={closeModal} />
  </Root>

export default () => {
  const dispatch = useDispatch();
  const projects = useSelector(projectsSelector);
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
