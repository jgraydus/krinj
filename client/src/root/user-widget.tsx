import type { ReactNode } from 'react'
import {  useCallback, useState } from 'react'
import styled from 'styled-components'

const Root = styled.div`
  height: 40px;
  width: 40px;
  position: relative;
`
const Handle = styled.div`
  cursor: pointer;
  height: 40px;
  width: 40px;
  border: 1px solid red;
`
const MenuRoot = styled.div`
  cursor: pointer;
  width: 100px;
  border: 1px solid red;
  background-color: white;
  position: absolute;
  right: 0;
  top: 40px;
  display: flex;
  flex-direction: column;
`
const Menu = ({ children }: { children: ReactNode }) => {
  return <MenuRoot>{children}</MenuRoot>
}
const MenuItem = styled(({ children, className }: { children: ReactNode, className?: string }) => {
  return <div className={className}>{children}</div>
})`
   height: 30px;
   width: 100%;
   display: flex;
   align-items: center;
   padding-left: 5px;
`
export default () => {
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const toggle = useCallback(() => {
    setIsMenuOpen(!isMenuOpen)
  }, [isMenuOpen]);

  return (
      <Root> 
         <Handle onClick={toggle} />
         {isMenuOpen && (
           <Menu>
             <MenuItem>Log Out</MenuItem>
             <MenuItem>Settings</MenuItem>
           </Menu>
         )}
      </Root>
  );
}

