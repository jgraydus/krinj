import { Link, Route, Routes } from 'react-router-dom'

import Issues from './issues'
import Projects from './projects'
import Project from './project'
import RootPage from './root-page'

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

