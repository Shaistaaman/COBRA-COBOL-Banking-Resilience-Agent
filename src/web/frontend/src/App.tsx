import { useState } from 'react'
import CodeEditor from './components/CodeEditor'
import FileUpload from './components/FileUpload'
import ProgressIndicator from './components/ProgressIndicator'
import ResultsDisplay from './components/ResultsDisplay'
import ExampleSnippets from './components/ExampleSnippets'
import './App.css'

interface AnalysisResult {
  id: string
  status: 'processing' | 'complete' | 'error'
  explanation?: string
  architecture?: string
  artifacts?: {
    lambda: boolean
    apiGateway: boolean
    cdk: boolean
  }
  error?: string
}

function App () {
  const [cobolCode, setCobolCode] = useState('')
  const [analysisId, setAnalysisId] = useState<string | null>(null)
  const [result, setResult] = useState<AnalysisResult | null>(null)
  const [isAnalyzing, setIsAnalyzing] = useState(false)

  const handleAnalyze = async () => {
    if (!cobolCode.trim()) {
      alert('Please enter COBOL code to analyze')
      return
    }

    setIsAnalyzing(true)
    setResult(null)

    try {
      // Start analysis
      const response = await fetch('/api/analyze', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ cobolSource: cobolCode })
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Analysis failed')
      }

      const { analysisId: id } = await response.json()
      setAnalysisId(id)

      // Poll for results
      pollAnalysisStatus(id)
    } catch (error: any) {
      alert(`Error: ${error.message}`)
      setIsAnalyzing(false)
    }
  }

  const pollAnalysisStatus = async (id: string) => {
    const maxAttempts = 60 // 60 seconds max
    let attempts = 0

    const poll = async () => {
      try {
        const response = await fetch(`/api/analysis/${id}`)
        if (!response.ok) throw new Error('Failed to fetch analysis status')

        const data: AnalysisResult = await response.json()
        setResult(data)

        if (data.status === 'complete' || data.status === 'error') {
          setIsAnalyzing(false)
          return
        }

        attempts++
        if (attempts < maxAttempts) {
          setTimeout(poll, 1000)
        } else {
          setIsAnalyzing(false)
          alert('Analysis timeout. Please try again.')
        }
      } catch (error: any) {
        setIsAnalyzing(false)
        alert(`Error: ${error.message}`)
      }
    }

    poll()
  }

  const handleFileUpload = (content: string) => {
    setCobolCode(content)
  }

  const handleExampleSelect = (code: string) => {
    setCobolCode(code)
    setResult(null)
    setAnalysisId(null)
  }

  return (
    <div className='app'>
      <header className='header'>
        <div className='header-content'>
          <img src='/logo_final.png' alt='COBRA Logo' className='logo' />
          <div className='header-text'>
            <h1>COBRA</h1>
            <p>COBOL Banking Resilience Agent</p>
          </div>
        </div>
      </header>

      <main className='main'>
        <div className='input-section'>
          <div className='controls'>
            <FileUpload onFileUpload={handleFileUpload} />
            <button
              className='analyze-btn'
              onClick={handleAnalyze}
              disabled={isAnalyzing || !cobolCode.trim()}
            >
              {isAnalyzing ? 'Analyzing...' : 'Analyze COBOL'}
            </button>
          </div>

          <ExampleSnippets onSelectExample={handleExampleSelect} />

          <CodeEditor value={cobolCode} onChange={setCobolCode} />
        </div>

        <div className='output-section'>
          {isAnalyzing && <ProgressIndicator />}
          {result && <ResultsDisplay result={result} analysisId={analysisId} />}
        </div>
      </main>
    </div>
  )
}

export default App
