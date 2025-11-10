import Editor from '@monaco-editor/react'
import './CodeEditor.css'

interface CodeEditorProps {
  value: string
  onChange: (value: string) => void
}

function CodeEditor ({ value, onChange }: CodeEditorProps) {
  return (
    <div className='code-editor'>
      <h3>COBOL Source Code</h3>
      <Editor
        height='500px'
        defaultLanguage='cobol'
        value={value}
        onChange={value => onChange(value || '')}
        theme='vs-dark'
        options={{
          minimap: { enabled: false },
          fontSize: 14,
          lineNumbers: 'on',
          scrollBeyondLastLine: false,
          automaticLayout: true
        }}
      />
    </div>
  )
}

export default CodeEditor
