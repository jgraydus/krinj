import { Link, Route, Routes } from 'react-router-dom'

import Issues from './issues'
import Projects from './projects'
import RootPage from './root-page'

const Project = ({projectId}) => <div>{projectId}</div>

export default () =>
  <RootPage>
     <Routes>
       <Route path="/projects">
         <Route path="" element={<Projects />} />
         <Route path=":projectId" element={<Project />} />
       </Route>
       <Route path="/issues" element={<Issues />} />
     </Routes>
  </RootPage>

