import { Link, Route, Routes } from 'react-router-dom'

import Issues from './issues'
import Project from './project'
import Projects from './projects'
import Root from './root'

export default () =>
  <Root>
     <Routes>
       <Route path="/projects">
         <Route path="" element={<Projects />} />
         <Route path=":projectId" element={<Project />} />
       </Route>
       <Route path="/issues" element={<Issues />} />
     </Routes>
  </Root>

