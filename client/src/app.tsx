import { Link, Navigate, Route, Routes } from 'react-router-dom'

import Issue from './projects/project/issues/issue'
import { Project, Issues } from './projects/project'
import Projects from './projects'
import Root from './root'

export default () =>
  <Root>
     <Routes>
       <Route path="/" element={<Navigate to="/projects" />} />
       <Route path="/projects">
         <Route path="" element={<Projects />} />
         <Route path=":projectId">
           <Route path="" element={<Project />} />
           <Route path="issues">
             <Route path="" element={<Issues />} />
             <Route path=":entityId" element={<Issue />} />
           </Route>
         </Route>
       </Route>
     </Routes>
  </Root>

